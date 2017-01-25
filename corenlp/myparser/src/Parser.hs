{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}


module Parser where

-- base
import Control.Applicative

-- directory
-- import System.Directory (getDirectoryContents,getCurrentDirectory)

-- filepath
-- import System.FilePath (splitExtension)

-- base

-- import System.IO ( hGetContents, hClose, hPutStrLn, stdout, stdin, stderr
--                 , withFile
--                 , readFile,openFile,IOMode(ReadMode,WriteMode)) -- writeFile


data ParserState s a = Failed s
                     | Partial (s -> ParserState s a)
                     | Success a s
                     -- deriving Show

-- deriving instance (Show s,Show a) => Show (ParserState s a)

deriving instance Functor (ParserState s)

newtype Parser a = Parser { unParser :: String -> ParserState String a }

deriving instance Functor Parser

instance Applicative Parser where
  --pure :: a -> m a
  pure x = Parser (\s -> Success x s)

  -- <*> :: f (a -> b) -> f a -> f b
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  fab <*> fa =  Parser $ \s -> case unParser fab s of
                                 Failed _ -> Failed s
                                 Partial pab -> Partial (\sf -> unParser (Parser pab <*> fa) sf) 
                                 Success ab s' ->
                                   case unParser fa s' of
                                     Failed _ -> Failed s'
                                     Partial pa -> Partial (\sf -> unParser (ab <$> Parser pa) sf)
                                     Success a s'' -> Success (ab a) s''

instance Alternative Parser where
  -- empty :: f a
  empty = fail "no"
  fa <|> fb = Parser $ \s -> case unParser fa s of
                               Success a s' -> Success a s'
                               Partial pa -> Partial (\sf -> unParser (Parser pa <|> fb) sf) 
                               Failed _ ->
                                 case unParser fb s of
                                   Success b s'' -> Success b s''
                                   Partial pb -> Partial pb
                                   Failed s'' -> Failed s


instance Monad Parser where
  -- (>>=) :: f a -> (a -> f b) -> f b
  fa >>= afb = Parser $ \s -> case unParser fa s of
                                Failed _ -> Failed s
                                Partial pa -> Partial (\sf -> unParser (Parser pa >>= afb) sf)
                                Success a s' -> unParser (afb a) s'
  fail str = Parser (\s -> Failed s)

-- fail :: Parser a
-- fail = 

parseOnly :: Parser a -> String -> Maybe a
parseOnly p str = case (unParser p str) of
                    Failed _ -> Nothing
                    Partial _ -> Nothing
                    Success a _ -> Just a

parse = unParser

parseBuffer :: Parser a -> [String] -> Maybe a
parseBuffer p [] = Nothing
parseBuffer p (x:xs) = case parse p x of
                         Success a _ -> Just a
                         Failed _ -> Nothing
                         Partial pa -> parseBuffer (Parser pa) xs



anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
                           [] -> Partial (\sf -> unParser anyChar sf)
                                  --  Failed []
                           (x:xs) -> Success x xs

-- String = [Char]
{-
char :: Char -> Parser Char
char c = do
  x <- anyChar
  if c == x
    then return c
    else fail "char"
-}

inClass :: (Char -> Bool) -> Parser Char
inClass p = do
  x <- anyChar
  if p x then return x else fail "inClass"

-- equivalent to inClass (\x->c==x)
char c = inClass (c==)



literal :: Parser String
literal = do char '"' 
             x <- many (inClass (not . (== '"'))) 
             char '"'
             return x

isSpace x = x == ' ' || x == '\n'  || x == '\r' || x == '\t'

skipSpace :: Parser ()
skipSpace = do many (inClass isSpace)
               return ()

keyValue :: Parser (String,Value)
keyValue = do k <- literal
              skipSpace
              char ':'
              skipSpace
              v <- value -- literal
              return (k,v)

data Value = VString String
           | VObject [(String,Value)]
           | VArray [Value]
           deriving Show

object = VObject <$> braces (sepBy ',' (skipSpace *> keyValue <* skipSpace))

array = VArray <$> brackets (sepBy ',' (skipSpace *> value <* skipSpace))

value = (VString <$> literal) <|> object <|> array


braces :: Parser a -> Parser a
braces p = char '{' *> skipSpace *> p <* skipSpace <* char '}'

brackets :: Parser a -> Parser a
brackets p = char '[' *> skipSpace *> p <* skipSpace <* char ']'


sepBy :: Char -> Parser a -> Parser [a]
sepBy c p = do r <- p
               (do char c 
                   rs <- sepBy c p
                   return (r:rs)
                <|> return [r])


{- 
braces p = do char '{'
              r <- p
              char '}'
              return r
-}


{- 
fabc <$> fa <*> fb 
               


 ac <$> fa <*> fb 
 a-> _ -> c          a      b
  

 ac <$> fa <* fb

 bc <$> fa *> fb

>>

 :: f c -}
