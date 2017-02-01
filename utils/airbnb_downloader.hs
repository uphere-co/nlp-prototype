import Network.HTTP
import Text.HTML.TagSoup

openURL url = getResponseBody =<< simpleHTTP (getRequest url)

main :: IO ()
main = do
  src <- openURL "http://insideairbnb.com/get-the-data.html"
  writeFile "get-the-data.html" src
