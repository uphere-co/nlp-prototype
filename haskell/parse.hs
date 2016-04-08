
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.XML.Types
import           Text.XML.Stream.Parse

filename = "ipg160105.xml"

main = do
  putStrLn "xml parse"

  x<- runResourceT $
    parseFile def filename $$ {- CL.isolate 10000 =$= -} (myprocess 0) -- CL.take 100 

  print x


findBegin tag success def = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just x -> do
      case x of
        EventBeginElement nm clst ->
          if nameLocalName nm == tag then success else def
        _ -> def


myprocess n = findBegin "us-patent-grant" action (myprocess n) >> myprocess (n+1)

  where action = do
          liftIO (putStrLn (show n ++ ": " ++ "us-patent-grant"))
          singlepatent n
          -- myprocess (n+1)


singlepatent n = do
  findBegin "doc-number" getDocNumber (singlepatent n)
  -- singleDescription

{- 
singleDescription = 
  findBegin "description" (liftIO (print "description")) singleDescription
-}
 
getDocNumber = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just x -> do
      case x of
        EventEndElement "doc-number" -> return ()
        EventContent (ContentText dn) ->  liftIO (print dn)
        _ -> getDocNumber

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