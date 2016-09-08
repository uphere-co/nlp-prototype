{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Types where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           Data.Map                 (Map)
import qualified Data.Map            as M
--
import           Prelude hiding (lookup)


data BinTreeF a r = BinNodeF r r
                  | BinLeafF a
                  deriving (Functor, Foldable, Traversable)

data BNTTreeF a r = BNTNodeF a r r
                  | BNTLeafF a

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


binprint :: (Show a) => BinTreeR a -> String
binprint (Fix (BinNodeF l r)) = "(" ++ binprint l ++ "," ++ binprint r ++ ")"
binprint (Fix (BinLeafF a))   = show a



