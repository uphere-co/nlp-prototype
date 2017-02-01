import           Control.Monad               (forM_)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP
import           Network.URI                 (parseURI)
import           Text.HTML.TagSoup

openURL url = getResponseBody =<< simpleHTTP (getRequest url)

downloadFile url = do
  let uri = parseURI url
  case uri of
    Nothing -> return ("Invalid URL: " ++ url)
    Just u  -> simpleHTTP (defaultGETRequest_ u) >>= getResponseBody

main :: IO ()
main = do
  let target = openURL "http://insideairbnb.com/get-the-data.html"
  tags <- fmap parseTags target
  let links = filter (~== TagOpen "a" []) tags
  
  forM_ links $ \link -> do
    let flink = fromAttrib "href" link
    print flink
    file <- downloadFile flink
    B.writeFile "ha" (B.pack file)
