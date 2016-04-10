
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
    Nothing -> return ()
    Just x -> do
      case x of
        EventBeginElement nm clst ->
          if nameLocalName nm == tag then begin else findBegin tag begin
        _ -> findBegin tag begin

findEnd tag inner = do  
  mx <- await
  case mx of
    Nothing -> return ()
    Just x -> do
      case x of
        EventEndElement nm ->
          when (nameLocalName nm /= tag) (findEnd tag inner)
        _ -> inner x >> findEnd tag inner
 
myprocess n = do
     findBegin "us-patent-grant" $ do
       liftIO (putStrLn (show n ++ ": " ++ "us-patent-grant"))
       singlepatent n
     myprocess (n+1)

applicationRef = findBegin "application-reference" $ do
                   liftIO (print "application-reference")  
                   findEnd "application-reference" (\_ -> return ())
          
singlepatent n = do
  findBegin "doc-number" $
    findEnd "doc-number" getDocNumber 
  applicationRef
  -- singleDescription

{- 
singleDescription = 
  findBegin "description" (liftIO (print "description")) singleDescription
-}

getDocNumber :: (MonadIO m) => Event -> Sink Event m ()
getDocNumber x = 
  case x of
    EventContent (ContentText dn) ->  liftIO (print dn)
    _ -> return ()


{-  
getDescription = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just x -> do
      case x of
        EventEndElement "description" -> liftIO (print "description") >> return ()
        _ -> getDescription
  -}
