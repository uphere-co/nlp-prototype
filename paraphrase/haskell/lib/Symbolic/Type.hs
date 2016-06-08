{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Symbolic.Type where

import           Data.Monoid
import           Data.Text (Text (..))
import qualified Data.Text as T
--
import NLP.SyntaxTree.Type

data BiOp = Add | Mul deriving (Show, Eq)

data UniOp = Tanh deriving (Show, Eq)

data Symbol = X | Y deriving (Show, Eq)

data Exp = Fun UniOp Exp
         | Var Symbol
         | Val Int
         | BiExp (BNTree BiOp Exp)
         deriving (Show, Eq)

deriving instance Eq (BNTree BiOp Exp)

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
