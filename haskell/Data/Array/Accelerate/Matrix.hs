{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.Matrix where

import           Data.Array.Accelerate             (use, (:.)(..), Array(..), Z(..),
                                                    DIM1, DIM2, All(..), Acc, Exp)
import qualified Data.Array.Accelerate      as A

type Matrix a = Array DIM2 a

matMul :: (A.IsNum e, A.Elt e) => Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
matMul arr brr = A.fold (+) 0 $ A.zipWith (*) arrRepl brrRepl
  where
    Z :. rowsA :. _     = A.unlift (A.shape arr) :: Z :. Exp Int :. Exp Int
    Z :. _     :. colsB = A.unlift (A.shape brr) :: Z :. Exp Int :. Exp Int
    arrRepl = A.replicate (A.lift $ Z :. All   :. colsB :. All ) arr
    brrRepl = A.replicate (A.lift $ Z :. rowsA :. All   :. All ) (A.transpose brr)


matTrace :: (A.IsNum e, A.Elt e) => Acc (Matrix e) -> Acc (A.Scalar e)
matTrace arr = A.sum $ A.slice y (A.lift (Z :. (0 :: Int) :. All))
  where
    Z :. rows :. _cols = A.unlift (A.shape arr) :: Z :. Exp Int :. Exp Int
    f = A.lift1  (\(Z :. i :. j) ->  Z :. (i-j) `mod` rows :. j)
    y = A.backpermute (A.lift (A.shape arr)) f arr
