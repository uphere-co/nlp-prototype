{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad               (forM_)
import qualified Data.ByteString    as B
import qualified Data.ByteString.Lazy as BL
import           Data.List                   (isInfixOf,isPrefixOf)
import           Data.List.Split             (splitOn)
import           Network.Curl
import           Network.HTTP
import           Network.HTTP.Client
import           Network.URI                 (parseURI)
import           Text.HTML.TagSoup

openURL url = getResponseBody =<< simpleHTTP (getRequest url)

downloadFile url = do
  let uri = parseURI url
  case uri of
    Nothing -> return ("Invalid URL: " ++ url)
    Just u  -> if (isInfixOf "insideairbnb" url == False)
               then return "F"
               else simpleHTTP (defaultGETRequest_ u) >>= getResponseBody

checkURL url = do
  if (isInfixOf "insideairbnb" url == False)
  then return "F"
  else return url

main :: IO ()
main = do
  man <- newManager defaultManagerSettings
  let target = openURL "http://insideairbnb.com/get-the-data.html"
  tags <- fmap parseTags target
  let links = filter (~== TagOpen ("a" :: String) []) tags
  forM_ links $ \link -> do
    let flink = fromAttrib "href" link
    file <- checkURL flink
    print file
    if file == "F"
      then return ()
      else do
      initReq <- parseRequest file
      let req = initReq { method = "GET" }
      response <- httpLbs req man
      let filename = last $ splitOn "/" file
      BL.writeFile filename (responseBody response)
    -- res <- ND.openURI file
    -- (_, txt) <- curlGetString file []
    -- let filename = last $ splitOn "/" file
    -- B.writeFile filename (B8.pack txt)
    {-
    case res of
      Left msg   -> putStrLn msg
      Right res' -> B.writeFile filename res'
    -}
