{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.Types where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.Flip
import           Data.Bitraversable
import           Data.Map                 (Map)
import qualified Data.Map            as M
import           Data.Monoid              ((<>))
--
import           Prelude hiding (lookup)


data BinTreeF a r = BinNodeF r r
                  | BinLeafF a
--                   deriving (Functor, Foldable, Traversable)

instance Bifunctor BinTreeF where
  bimap f g (BinNodeF l r) = BinNodeF (g l) (g r)
  bimap f g (BinLeafF x)   = BinLeafF (f x) 

instance Bifoldable BinTreeF where
  bifoldMap f g (BinNodeF l r) = g l <> g r
  bifoldMap f g (BinLeafF x)   = f x
  
instance Bitraversable BinTreeF where
  bitraverse f g (BinNodeF l r) = BinNodeF <$> g l <*> g r
  bitraverse f g (BinLeafF x)   = BinLeafF <$> f x


data BNTTreeF a r = BNTNodeF a r r
                  | BNTLeafF a


instance Bifunctor BNTTreeF where
  bimap f g (BNTNodeF x l r) = BNTNodeF (f x) (g l) (g r)
  bimap f g (BNTLeafF x)     = BNTLeafF (f x)

instance Bifoldable BNTTreeF where
  bifoldMap f g (BNTNodeF x l r) = f x <> g l <> g r
  bifoldMap f g (BNTLeafF x)     = f x

instance Bitraversable BNTTreeF where
  bitraverse f g (BNTNodeF x l r) = BNTNodeF <$> f x <*> g l <*> g r
  bitraverse f g (BNTLeafF x)     = BNTLeafF <$> f x

newtype Fix f = Fix (f (Fix f))

type BinTreeR a = Fix (BinTreeF a)

type BNTTreeR a = Fix (BNTTreeF a)

newtype Ref = Ref Int deriving (Eq,Ord,Show,Num)

data TreeState a = TreeState { nextRef :: Ref
                             , currentGraph :: Map Ref (BinTreeF a Ref)
                             -- , currentTree :: BinTreeR a
                             }

emptyTS :: TreeState a
emptyTS = TreeState 0 M.empty
                   
type TreeM a m = EitherT String (StateT (TreeState a) m)

push :: (Monad m) => BinTreeF a Ref -> TreeM a m Ref
push t = lift $ do TreeState r m <- get
                   put (TreeState (r+1) (M.insert r t m))
                   return r

lookup :: (Monad m) => Ref -> TreeM a m (BinTreeF a Ref)
lookup r = maybe (left ("No such ref" ++ show r)) right . M.lookup r =<< lift (currentGraph <$> get)

binnodef :: r -> r -> BinTreeF a r
binnodef l r = BinNodeF l r

binleaff :: a -> BinTreeF a r
binleaff a = BinLeafF a

binnode :: BinTreeR a -> BinTreeR a -> BinTreeR a 
binnode l r = Fix (binnodef l r)

binleaf :: a -> BinTreeR a
binleaf a = Fix (binleaff a)

binnodeM :: (Monad m) => Ref -> Ref -> TreeM a m Ref
binnodeM l r = do lookup l
                  lookup r
                  push (binnodef l r)

binleafM :: (Monad m) => a -> TreeM a m Ref
binleafM a = push (binleaff a)




-- non-generic version. 
--
-- graph2tree :: Map Ref (BinTreeF a Ref) -> Ref -> Either String (BinTreeR a)
-- graph2tree m r = do f <- elookup r m
--                     case f of
--                       BinNodeF l r -> binnode <$> graph2tree m l <*> graph2tree m r
--                       BinLeafF a -> return (binleaf a)
--   where elookup r = maybe (Left ("no such " ++ show r)) Right . M.lookup r


-- | generic graph2tree
graph2tree :: (Bitraversable f) => Map Ref (f a Ref) -> Ref -> Either String (Fix (f a))
graph2tree m r = do f <- elookup r m
                    Fix <$> bitraverse pure (graph2tree m) f
 where elookup r = maybe (Left ("no such " ++ show r)) Right . M.lookup r
                      
       
binprint :: (Show a) => BinTreeR a -> String
binprint (Fix (BinNodeF l r)) = "(" ++ binprint l ++ "," ++ binprint r ++ ")"
binprint (Fix (BinLeafF a))   = show a



