module NLP.SyntaxTree.Parser where

import           Data.Text                   (Text(..))
import           Control.Applicative
import qualified Data.Attoparsec.Text as A
--
import           NLP.SyntaxTree.Type

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
tag = A.takeWhile (`elem` ([ 'A'..'Z' ] ++ ['0'..'9'] ++ ".,-:'`"))

