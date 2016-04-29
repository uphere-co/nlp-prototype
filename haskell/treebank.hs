{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative         ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable        as F
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text(..))
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
--
import           Binarize
import           Printer
import           Types


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

oparen :: A.Parser Char
oparen = A.char '('

cparen :: A.Parser Char
cparen = A.char ')'

tag :: A.Parser Text
tag = A.takeWhile (`elem` ([ 'A'..'Z' ] ++ ".,"))


main :: IO ()
main = do
  txt <- TIO.readFile "parsed.txt"
  let p' = penntree <* A.skipSpace 
  let r = A.parseOnly (A.many1 p') txt
  case r of
    Left err -> print err
    Right lst -> do
      F.forM_ (take 1 lst) $ \tr -> do
        print tr 
        putStrLn "-----"
        TIO.putStrLn (treeprinter 0 tr)
        putStrLn "-----"
        let btr = binarizeR tr
        print btr
        putStrLn "-----"
        TIO.putStrLn $ btreeprinter 0 btr -- (binarizeR tr)
        putStrLn "=====" 
        
