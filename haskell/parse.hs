
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

filename = "ipg160105.xml"

main = do
  putStrLn "xml parse"

  runResourceT $
    parseFile def filename $$ {- CL.isolate 10000 =$= -} (myprocess 0) -- CL.take 100 

findBegin tag begin = do
  mx <- await
  case mx of
    Nothing -> return False
    Just x -> do
      case x of
        EventBeginElement nm clst ->
          if nameLocalName nm == tag then begin >> return True else findBegin tag begin
        _ -> findBegin tag begin

findEnd tag inner = do  
  mx <- await
  case mx of
    Nothing -> return False
    Just x -> do
      case x of
        EventEndElement nm -> do
          if nameLocalName nm == tag then return True else findEnd tag inner
        _ -> inner x >> findEnd tag inner

myprocess n = do
     r <- findBegin "us-patent-grant" $ do
       liftIO (putStrLn (show n ++ ": " ++ "us-patent-grant"))
       singlepatent n
     if r then myprocess (n+1) else return ()

singlepatent n = do
  docNumber 
  applicationRef
  description

docNumber = findBegin "doc-number" $ findEnd "doc-number" getDocNumber 

applicationRef = findBegin "application-reference" $ do
                   liftIO (print "application-reference")  
                   findEnd "application-reference" (\_ -> return ())

description = findBegin "description" $ do
                liftIO $ print "found description"
                findEnd "description" getDescription
          
getDocNumber :: (MonadIO m) => Event -> Sink Event m ()
getDocNumber x = 
  case x of
    EventContent (ContentText dn) ->  liftIO (print dn)
    _ -> return ()
  
getDescription x = do
  case x of
    EventContent (ContentText dn) ->  liftIO (putStr (T.unpack dn))
    _ -> return ()   
