
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

filename = "ipg160105.xml"

main = 
  runResourceT $
    parseFile def filename $$ {- CL.isolate 10000 =$= -} runStateT myprocess 0 -- CL.take 100 

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

myprocess :: (MonadIO m) => StateT Int (Sink Event m) ()
myprocess = do
     n <- get
     r <- findBegin "us-patent-grant" $ do
       liftIO (putStrLn (show n ++ ": " ++ "us-patent-grant"))
       singlepatent
     if r then (put (n+1) >> myprocess) else return ()

singlepatent :: (MonadIO m) => StateT Int (Sink Event m) Bool
singlepatent = do
  docNumber 
  applicationRef
  description

docNumber :: (MonadIO m) => StateT Int (Sink Event m) Bool
docNumber = findBegin "doc-number" $ findEnd "doc-number" getDocNumber 

applicationRef :: (MonadIO m) => StateT Int (Sink Event m) Bool
applicationRef = findBegin "application-reference" $ do
                   liftIO (print "application-reference")  
                   findEnd "application-reference" (\_ -> return ())

description :: (MonadIO m) => StateT Int (Sink Event m) Bool
description = findBegin "description" $ do
                liftIO $ print "found description"
                findEnd "description" getDescription
          
getDocNumber :: (MonadIO m) => Event -> StateT Int (Sink Event m) ()
getDocNumber x = 
  case x of
    EventContent (ContentText dn) ->  liftIO (print dn)
    _ -> return ()
  
getDescription x = do
  case x of
    EventContent (ContentText dn) ->  liftIO (putStr (T.unpack dn))
    _ -> return ()   
