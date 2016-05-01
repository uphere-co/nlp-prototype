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

{- 
instance Applicative BinTree where
  pure = BinLeaf
  BinLeaf f <*> BinLeaf x       = BinLeaf (f x)
  BinLeaf f <*> BinNode x y     = BinNode (f <$> x) (f <$> y)
  BinNode f1 f2 <*> BinLeaf x   = BinNode (f1 <*> pure x) (f2 <*> pure x)
  BinNode f1 f2 <*> BinNode x y = BinNode (f1 <*> x) (f2 <*> y)

instance Monad BinTree where
  return = BinLeaf
  (>>=) = undefined
-}

data BNTree e a = BNTNode e (BNTree e a) (BNTree e a)
                | BNTLeaf a
                deriving (Functor, Foldable, Traversable)

deriving instance (Show e,Show a) => Show (BNTree e a)


