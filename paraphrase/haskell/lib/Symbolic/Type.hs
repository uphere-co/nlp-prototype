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

-- data UniOp = Tanh | Sech deriving (Show, Eq)
type UniOp = Text

data Symbol = X | Y deriving (Show, Eq)

data Exp = Fun UniOp Exp
         | Var Symbol
         | Val Int
         | BiExp (BNTree BiOp Exp)
         | Zero
         | One
         deriving (Show, Eq)

deriving instance Eq (BNTree BiOp Exp)

lift = BiExp

showBiOp Add = "+"
showBiOp Mul = "*"

-- showUniOp Tanh = "tanh"
-- showUniOp Sech = "sech"
showUniOp = id

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
prettyprint Zero = "zero"
prettyprint One = "1"

-- gate keeper, simplifying cases with zero
add :: Exp -> Exp -> Exp
add Zero e2 = e2
add e1 Zero = e1
add e1 e2   = BiExp (BNTNode Add (BNTLeaf e1) (BNTLeaf e2))

mul :: Exp -> Exp -> Exp
mul Zero e2   = Zero
mul e1   Zero = Zero
mul One  e2   = e2
mul e1   One  = e1
mul e1   e2   = BiExp (BNTNode Mul (BNTLeaf e1) (BNTLeaf e2))

(/+/) = add
(/*/) = mul
