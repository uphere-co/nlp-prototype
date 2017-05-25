module Misc where

import qualified Data.Vector                   as V
import           Data.Vector                           (Vector,toList,fromList,ifoldr,foldl')

data IRange = IRange { beg :: Int
                     , end :: Int}
                deriving(Eq, Show)


isContain :: Eq a => Vector a -> Vector a -> Bool
isContain = f 
  where
    zipEq x y = V.all (uncurry (==)) (V.zip x y)
    f sub vec | V.length sub > V.length vec = False
    f sub vec | zipEq sub vec = True
    f sub vec                 = f sub (V.tail vec)

subVector :: IRange -> Vector a -> Vector a
subVector (IRange beg end) = V.slice beg (end-beg)