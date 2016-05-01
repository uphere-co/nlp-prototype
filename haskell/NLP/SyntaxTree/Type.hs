{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.SyntaxTree.Type where

import           Data.Text                   (Text(..))

data PennTree = PT Text [PennTree]
              | PN Text
              deriving Show

data BinTree a = BinNode (BinTree a) (BinTree a)
               | BinLeaf a
               deriving (Functor, Foldable, Traversable) 

deriving instance (Show a) => Show (BinTree a)

data BNTree e a = BNTNode e (BNTree e a) (BNTree e a)
                | BNTLeaf a
                deriving (Functor, Foldable, Traversable)

deriving instance (Show e,Show a) => Show (BNTree e a)


