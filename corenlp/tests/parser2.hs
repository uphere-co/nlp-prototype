{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- base
import Control.Applicative

-- directory
import System.Directory (getDirectoryContents,getCurrentDirectory)

-- filepath
import System.FilePath (splitExtension)

-- base
import System.IO ( hGetContents, hClose, hPutStrLn, stdout, stdin, stderr
                 , withFile
                 , readFile,openFile,IOMode(ReadMode,WriteMode)) -- writeFile


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

char c = inClass (\x -> c == x)



literal :: Parser String
literal = do char '"' 
             x <- many (inClass (not . (== '"'))) 
             char '"'
             return x

skipSpace :: Parser ()
skipSpace = do many (inClass (== ' '))
               return ()

keyValue :: Parser (String,String)
keyValue = do skipSpace
              k <- literal
              skipSpace
              char ':'
              skipSpace
              v <- literal
              return (k,v)



               
main' = do
  print (parseOnly (char '1') "1sdlfjdslkjsd")

  print (parseOnly (many anyChar) "sldkfjfsldfjsdl")
  print (parseOnly (many anyChar) "")
  print (parseOnly (some anyChar) "")

  print (parseOnly (some anyChar) "abcd")

  print (parseOnly literal "\"sddlfkjsdlfjie\"")
  print (parseOnly (skipSpace >> literal) "       \"abcdsddlfkjsdlfjie\"")

  print (parseOnly (keyValue) "       \"key\"  : \"value\"   ")

  case (parse keyValue  "       \"key\"  : \"value") of
    Failed s -> print s
    Success a s -> print (a,s)
    Partial f ->
      case (f "slsl  \" ") of
        Failed s -> print s
        Success a s -> print (a,s)
        Partial f' -> print "partial"

  print (parseBuffer keyValue [ "       \"key\"  : \"value", "slsl  \" "] )

-- type FilePath = String

main = do
  cwd <- getCurrentDirectory
  contents <- getDirectoryContents cwd
  let xs = map splitExtension contents
  print xs
  -- str <- readFile "monad.hs"
  -- putStrLn str
  {- 
  h <- openFile "monad.hs" ReadMode
  str' <- hGetContents h
  putStrLn str'
  hClose h
  -}
  withFile "monad.hs" ReadMode $ \h -> do
    str' <- hGetContents h
    putStrLn str'

  withFile "test.txt" WriteMode $ \h -> do
    hPutStrLn h "abcdefg"
    hPutStrLn stdout "I am uphere"
    hPutStrLn stderr "I am uphere 2"
  str <- hGetContents stdin
  putStrLn str
