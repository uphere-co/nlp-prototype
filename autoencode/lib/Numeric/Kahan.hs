module Numeric.Kahan where

import qualified Data.List            as L
import           Data.Vector.Storable      (Vector)
import qualified Data.Vector.Storable as V

-- Naive summation is not good. We should reduce error using Kahan summation algorithm.

data KahanSum = KahanSum {-# UNPACK #-} !Float {-# UNPACK #-} !Float
              deriving (Eq, Show)

unKahan :: KahanSum -> Float
unKahan (KahanSum s _) = s

kahanAdd :: KahanSum -> Float -> KahanSum
kahanAdd (KahanSum s c) x = KahanSum s' c'
  where s' = s + y
        c' = (s' - s) - y
        y  = x - c

kahanZero :: KahanSum
kahanZero = KahanSum 0 0

kahanSumV :: Vector Float -> Float
kahanSumV = unKahan . V.foldl' kahanAdd kahanZero

kahanSumL :: [Float] -> Float
kahanSumL = unKahan .L.foldl' kahanAdd kahanZero
