{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import           Data.Monoid ((<>))
import           Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data PennTree = PT Text [PennTree]
              | PN Text
              deriving Show

penntree :: A.Parser PennTree
penntree =
    (do oparen
        t <- tag
        A.skipSpace
        s <- A.many1 (penntree <* A.skipSpace)
	A.skipWhile (/= ')')
        cparen 
        return (PT t s))
    <|> 
    (do s <- A.takeWhile1 (not . (`elem` ['(',')']))
        return (PN s))

oparen = A.char '('

cparen = A.char ')'

tag = A.takeWhile (`elem` ([ 'A'..'Z' ] ++ ".,"))

textprinter :: Int -> PennTree -> Text
textprinter n (PT _ lst) = T.intercalate "\n" (map (textprinter (n+4)) lst)
textprinter n (PN txt) = T.replicate n " " <> txt --  (T.unpack txt)

treeprinter :: Int -> PennTree -> Text
treeprinter n (PT t lst) = "\n" <> fmttag <> T.concat (map (treeprinter (n+2)) lst)
  where fmttag = T.replicate n " " <> T.take 4 (t <> "    ") <> " "
treeprinter n (PN txt) = txt


main :: IO ()
main = do
  txt <- TIO.readFile "parsed.txt"
  let p' = penntree <* A.skipSpace 
  let r = A.parseOnly (A.many1 p') txt
  case r of
    Right lst -> do
      mapM_ (\x -> print x >> putStrLn "-----" >> TIO.putStrLn (treeprinter 0 x) >> putStrLn "=====" )  lst -- (take 1 lst)
    Left err -> print err
