{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.SyntaxTree.Binarize where

import           Data.Text (Text)
import qualified Data.Text as T (replicate)
import           Data.Monoid
-- 
import NLP.SyntaxTree.Type

testbtree :: BinTree Text
testbtree = BinNode (BinNode (BinLeaf "a") (BinLeaf "b")) (BinLeaf "c")

binarizeR :: PennTree -> BinTree Text
binarizeR (PN t)    = BinLeaf t
binarizeR (PT _ xs) = go xs
 where
   go []       = error "impossible"
   go (x:[])   = binarizeR x 
   -- go (x:y:[]) = BinNode (binarizeR x) (binarizeR y)
   go (x:xs)   = BinNode (binarizeR x) (go xs)
