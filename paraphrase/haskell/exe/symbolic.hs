{-# LANGUAGE OverloadedStrings #-}

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
         deriving (Show, Eq)

test :: BNTree BiOp Exp
test = BNTNode Add (BNTNode Mul (BNTLeaf (Fun Tanh (Var X))) (BNTLeaf (Val 7))) (BNTLeaf (Val 4))

showBiOp Add = "+"
showBiOp Mul = "*"

showUniOp Tanh = "tanh"

showSymbol X = "x"
showSymbol Y = "y"

showExp (Val x) = T.pack (show x)
showExp (Var x) = showSymbol x
showExp (Fun o x) = showUniOp o <> "(" <> showExp x <> ")"

prettyprint :: BNTree BiOp Exp -> Text
prettyprint (BNTNode o x y) = "(" <> prettyprint x <> showBiOp o <> prettyprint y <> ")"
prettyprint (BNTLeaf x) = showExp x

pExp = pFun <|> pVal <|> pVar

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

pLeaf = BNTLeaf <$> pExp

pNode :: A.Parser (BNTree BiOp Exp)
pNode = do
    A.char '('
    x <- (pNode <|> pLeaf)
    o <- pBiOp
    y <- (pNode <|> pLeaf)
    A.char ')'
    return (BNTNode o x y)

main :: IO ()
main = do
    putStrLn "symbolic calculation test"
    print test
    TIO.putStrLn (prettyprint test)
  
    print (A.parseOnly pNode "((tanh(x)*7)+4)")

    
