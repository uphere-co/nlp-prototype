{-# LANGUAGE OverloadedStrings #-}

module Symbolic.Parser where

import           Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Char (isAlpha, isSpace)
import qualified Data.Text as T
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

pSymbol =  ((A.char 'x' >> return X) <|> (A.char 'y' >> return Y))

pIdxVar = do
   sym <- pSymbol
   A.char '_'
   A.char 'i'
   i <- A.decimal
   return (IdxVar sym (Idx i))

pVar = Var <$> (pIdxVar <|> (SmplVar <$> pSymbol))

pUniOp = do
    c <- A.satisfy (isAlpha)
    r <- A.takeTill (\x-> isSpace x || x == '(' || x == ')')
    return (c `T.cons` r)

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
