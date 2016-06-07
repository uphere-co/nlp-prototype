{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Monoid
import           Data.Text (Text (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
--
import           NLP.SyntaxTree.Type

data BiOp = Add | Mul deriving (Show, Eq)

-- data Exp = Op Operation
--          | Val Int

data UniOp = Tanh deriving (Show, Eq)

data Symbol = X | Y deriving (Show, Eq)

data Exp = Fun UniOp Exp
         | Var Symbol
         | Val Int
         | BiExp (BNTree BiOp Exp)
         deriving (Show, Eq)

-- type SymExp = BNTree BiOp Exp

deriving instance Eq (BNTree BiOp Exp)

test :: Exp -- BNTree BiOp Exp
test = BiExp $ BNTNode Add (BNTNode Mul (BNTLeaf (Fun Tanh (Var X))) (BNTLeaf (Val 7))) (BNTLeaf (Val 4))

showBiOp Add = "+"
showBiOp Mul = "*"

showUniOp Tanh = "tanh"

showSymbol X = "x"
showSymbol Y = "y"

showBiExp :: BNTree BiOp Exp -> Text
showBiExp (BNTNode o x y) = "(" <> showBiExp x <> showBiOp o <> showBiExp y <> ")"
showBiExp (BNTLeaf x) = prettyprint x

prettyprint :: Exp -> Text
prettyprint (Val x) = T.pack (show x)
prettyprint (Var x) = showSymbol x
prettyprint (Fun o x) = showUniOp o <> "(" <> prettyprint x <> ")"
prettyprint (BiExp n) = showBiExp n 

pExp = pFun <|> pVal <|> pVar <|> pBiExp

pFun = do
    o <- pUniOp
    A.char '('
    e <- pExp
    A.char ')'
    return (Fun o e) 

pVal = Val <$> A.decimal

pVar = Var <$> ((A.char 'x' >> return X) <|> (A.char 'y' >> return Y))

pUniOp = A.string "tanh" >> return Tanh

pBiOp = (A.char '+' >> return Add) <|> (A.char '*' >> return Mul)


pBiExp = BiExp <$> pNode  -- a little hole here.

pLeaf = BNTLeaf <$> pExp

pNode :: A.Parser (BNTree BiOp Exp)
pNode = do
    A.char '('
    x <- (pNode <|> pLeaf)
    o <- pBiOp
    y <- (pNode <|> pLeaf)
    A.char ')'
    return (BNTNode o x y)

-- diff :: Symbol -> BNTree BiOp Exp -> BNTree BiOp Exp



main :: IO ()
main = do
    putStrLn "symbolic calculation test"
    print test
    TIO.putStrLn (prettyprint test)
  
    print (A.parseOnly pExp "((tanh((x+y))*7)+4)")

    
