{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad               (forM_)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.List                   (isInfixOf,isPrefixOf)
import           Data.List.Split             (splitOn)
import           Data.Monoid                 (mconcat)
import           Network.Curl
import           Network.HTTP
import           Network.HTTP.Client
import           Network.URI                 (parseURI)
import           Text.HTML.TagSoup

openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

checkURL :: String -> IO (Maybe String)
checkURL url = do
  if (isInfixOf "insideairbnb" url == False)
  then return Nothing
  else return (Just url)

main :: IO ()
main = do
  man <- newManager defaultManagerSettings
  let target = openURL "http://insideairbnb.com/get-the-data.html"
  tags <- fmap parseTags target
  let links = filter (~== TagOpen ("a" :: String) []) tags

  forM_ links $ \link -> do
    let flink = fromAttrib "href" link
    file <- checkURL flink
    case file of
      Nothing -> return ()
      Just f' -> do
        print f'
        initReq <- parseRequest f'
        let req = initReq { method = "GET" }
        response <- httpLbs req man
        let filename = mconcat $ reverse $(take 6) $ reverse $ splitOn "/" f'
        BL.writeFile filename (responseBody response)
