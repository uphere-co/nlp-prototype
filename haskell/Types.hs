{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import           Data.Text                   (Text(..))

data PennTree = PT Text [PennTree]
              | PN Text
              deriving Show

data BinTree a = BinNode (BinTree a) (BinTree a)
               | BinLeaf a
               deriving (Functor, Foldable, Traversable) 

deriving instance (Show a) => Show (BinTree a)

