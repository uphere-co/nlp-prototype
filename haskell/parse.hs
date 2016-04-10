{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

filename = "ipg160105.xml"

data ParserState = ParserState { doc_serial :: Int
                               , doc_id :: Text
                               , doc_desc_text :: Text
                               , doc_desc_length :: Int }
                 deriving Show
                            
instance Default ParserState where
  def = ParserState 0 "" "" 0


main = 
  runResourceT $
    parseFile def filename $$ {- CL.isolate 10000 =$= -} runStateT myprocess def -- CL.take 100 

findBegin tag begin = do
  mx <- lift await
  case mx of
    Nothing -> return False
    Just x -> do
      case x of
        EventBeginElement nm clst ->
          if nameLocalName nm == tag then begin >> return True else findBegin tag begin
        _ -> findBegin tag begin

findEnd tag inner = do  
  mx <- lift await
  case mx of
    Nothing -> return False
    Just x -> do
      case x of
        EventEndElement nm -> do
          if nameLocalName nm == tag then return True else findEnd tag inner
        _ -> inner x >> findEnd tag inner

myprocess :: (MonadIO m) => StateT ParserState (Sink Event m) ()
myprocess = do
     state <- get
     let n = doc_serial state
     r <- findBegin "us-patent-grant" $ do
       liftIO (putStrLn (show n ++ ": " ++ "us-patent-grant"))
       singlepatent
     if r
       then do
         state' <- get
         liftIO $ print state'
         put (def { doc_serial = n+1 })
         myprocess
       else return ()

singlepatent :: (MonadIO m) => StateT ParserState (Sink Event m) Bool
singlepatent = do
  docNumber 
  applicationRef
  description

docNumber :: (MonadIO m) => StateT ParserState (Sink Event m) Bool
docNumber = findBegin "doc-number" $ findEnd "doc-number" getDocNumber 

applicationRef :: (MonadIO m) => StateT ParserState (Sink Event m) Bool
applicationRef = findBegin "application-reference" $ do
                   liftIO (print "application-reference")  
                   findEnd "application-reference" (\_ -> return ())

description :: (MonadIO m) => StateT ParserState (Sink Event m) Bool
description = findBegin "description" $ do
                liftIO $ print "found description"
                findEnd "description" getDescription
          
getDocNumber :: (MonadIO m) => Event -> StateT ParserState (Sink Event m) ()
getDocNumber x = 
  case x of
    EventContent (ContentText dn) -> modify' (\st -> st { doc_id = dn })
    _ -> return ()
  
getDescription x = do
  case x of
    EventContent (ContentText dn) ->  do
      let len = T.length dn
      modify' $ \st -> let n = doc_desc_length st
                           txt = doc_desc_text st
                       in st { doc_desc_length = n + len
                             -- , doc_desc_text = txt <> dn 
                             }
      -- liftIO (putStr (T.unpack dn))
    _ -> return ()   
