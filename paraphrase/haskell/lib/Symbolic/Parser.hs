{-# LANGUAGE OverloadedStrings #-}

module Symbolic.Parser where

import           Control.Applicative
import qualified Data.Attoparsec.Text as A
--
import           NLP.SyntaxTree.Type
import           Symbolic.Type

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
