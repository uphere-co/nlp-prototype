{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

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

data Index = Idx Int deriving (Show, Eq)

data Variable = SmplVar Symbol
              | IdxVar Symbol Index
              deriving (Show, Eq)

data Exp = Fun UniOp Exp
         | Var Variable
         | Val Int
         | BiExp (BNTree BiOp Exp)
         | Zero
         | One
         | Delta Index Index  -- Delta (i -> j). this is a directed graph.
         deriving (Show, Eq)

deriving instance Eq (BNTree BiOp Exp)

lift = BiExp

showBiOp Add = "+"
showBiOp Mul = "*"

showUniOp = id

showSymbol X = "x"
showSymbol Y = "y"

showVariable (SmplVar sym) = showSymbol sym
showVariable (IdxVar sym (Idx i)) = showSymbol sym <> "_i" <> T.pack (show i)

showBiExp :: BNTree BiOp Exp -> Text
showBiExp (BNTNode o x y) = "(" <> showBiExp x <> showBiOp o <> showBiExp y <> ")"
showBiExp (BNTLeaf x) = prettyprint x

prettyprint :: Exp -> Text
prettyprint (Val x) = T.pack (show x)
prettyprint (Var x) = showVariable x
prettyprint (Fun o x) = showUniOp o <> "(" <> prettyprint x <> ")"
prettyprint (BiExp n) = showBiExp n 
prettyprint Zero = "zero"
prettyprint One = "1"
prettyprint (Delta (Idx i) (Idx j)) = "delta(i" <> (T.pack (show i)) <> "->i" <> (T.pack (show j)) <> ")"

-- gate keeper, simplifying cases with zero
add :: Exp -> Exp -> Exp
add Zero e2 = e2
add e1 Zero = e1
add e1 e2   = BiExp (BNTNode Add (BNTLeaf e1) (BNTLeaf e2))

mul :: Exp -> Exp -> Exp
mul Zero           e2             = Zero
mul e1             Zero           = Zero
mul One            e2             = e2
mul e1             One            = e1
mul e1             d2@(Delta i j) = simplifyDelta i j e1
mul d1@(Delta i j) e2             = simplifyDelta i j e2
mul e1             e2             = BiExp (BNTNode Mul (BNTLeaf e1) (BNTLeaf e2))


-- | This function is inefficiently implemented. We should tag expression with free
--   variables

simplifyDelta i j (Val x)                           = Val x
simplifyDelta i j (Var x@(SmplVar _))               = Var x
simplifyDelta i j (Var x@(IdxVar s i')) | i == i'   = Var (IdxVar s j)
                                        | otherwise = Var x
simplifyDelta i j (Fun o e)                         = Fun o (simplifyDelta i j e)
simplifyDelta i j (BiExp b)                         = BiExp (sdbi i j b)
simplifyDelta i j e                                 = e

sdbi i j (BNTLeaf e) = BNTLeaf (simplifyDelta i j e)
sdbi i j (BNTNode o e1 e2) = BNTNode o (sdbi i j e1) (sdbi i j e2)

(/+/) = add
(/*/) = mul
