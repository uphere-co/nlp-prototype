

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
import Control.Monad.State

data ParserState a = Failed a
                   | Success a

deriving instance Show (ParserState String)

deriving instance Functor ParserState

type Parser a = State (ParserState String) a


parse :: Parser a -> String -> Maybe a
parse p str = case runState p (Success str) of
                (r,Success _) -> Just r
                (_,Failed _) -> Nothing



char :: Char -> Parser Char
char c = do m <- get
            case m of
              Failed xs -> undefined
              Success xs -> if c == head xs
                              then c , Success (last xs)
                              else ? , Failed xs


-- literal :: Parser String
-- literal 


main = do
  -- print (runState (get >>= put . fmap (++"abc")) (Success "1234"))
  let p = get >>= put . fmap (++"abc")
  print (parse p "1234") 
-}

import Control.Applicative

data ParserState s a = Failed s
                     | Success (a,s)

deriving instance (Show s,Show a) => Show (ParserState s a)

deriving instance Functor (ParserState s)

newtype Parser a = Parser { unParser :: String -> ParserState String a }

deriving instance Functor Parser

instance Applicative Parser where
  --pure :: a -> m a
  pure x = Parser (\s -> Success (x,s))

  -- <*> :: f (a -> b) -> f a -> f b
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  fab <*> fa =  Parser (\s -> case unParser fab s of
                                Failed _ -> Failed s
                                Success (ab,s') ->
                                  case unParser fa s' of
                                    Failed _ -> Failed s'
                                    Success (a,s'') -> Success (ab a,s'')) 

instance Alternative Parser where
  -- empty :: f a
  empty = fail "no"
  fa <|> fb = Parser (\s -> case unParser fa s of
                              Success (a,s') -> Success (a,s')
                              Failed _ ->
                                case unParser fb s of
                                  Success (b,s'') -> Success (b,s'')
                                  Failed s'' -> Failed s)


instance Monad Parser where
  -- (>>=) :: f a -> (a -> f b) -> f b
  fa >>= afb = Parser (\s -> case unParser fa s of
                               Failed _ -> Failed s
                               Success (a,s') -> unParser (afb a) s')
  fail str = Parser (\s -> Failed s)

-- fail :: Parser a
-- fail = 

parse :: Parser a -> String -> Maybe a
parse p str = case (unParser p str) of
                Failed _ -> Nothing
                Success (a,_) -> Just a

anyChar :: Parser Char
anyChar = Parser (\s -> case s of
                          [] -> Failed []
                          (x:xs) -> Success (x,xs))

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



               
main = do
  
  putStrLn "a"
  print (parse (char '1') "1sdlfjdslkjsd")


  print (parse (many anyChar) "sldkfjfsldfjsdl")
  print (parse (many anyChar) "")
  print (parse (some anyChar) "")

  print (parse (some anyChar) "abcd")

  print (parse literal "\"sddlfkjsdlfjie\"")
  print (parse (skipSpace >> literal) "       \"abcdsddlfkjsdlfjie\"")

  print (parse (keyValue) "       \"key\"  : \"value\"   ")
