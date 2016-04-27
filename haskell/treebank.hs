import           Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import           Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  txt <- TIO.readFile "parsed.txt"
  let p' = penntree <* A.skipSpace 
  let r = A.parseOnly (A.many1 p') txt -- (A.many1 (p <* A.skipSpace)) txt
  case r of
    Right lst -> print (length lst)
    Left err -> print err

data PennTree = PT Text [PennTree]
              | PN Text
              deriving Show


penntree :: A.Parser PennTree -- (Text,Text)
penntree =
    (do oparen
        t <- tag
        A.skipSpace
        s <- A.many1 (penntree <* A.skipSpace)
	A.skipWhile (/= ')')
        cparen 
        return (PT t s))
    <|> 
    (do s <- A.takeWhile1 (not . (`elem` "()"))
        return (PN s))

oparen = A.char '('

cparen = A.char ')'

tag = A.takeWhile (`elem` ([ 'A'..'Z' ] ++ ".,"))
