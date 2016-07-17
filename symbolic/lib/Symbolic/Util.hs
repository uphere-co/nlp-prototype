{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Symbolic.Util where

import Symbolic.Type
--
import Debug.Trace

indexSize (_,s,e) = e-s+1

-- | Factors needed for index flattening (column-major order)
indexFlatteningFactors :: [Index] -> [Int]
indexFlatteningFactors is = scanr (*) 1 (tail (map indexSize is))

flattenBy is fac = sum $ zipWith (\j f -> if f == 1 then j else j*f) is fac

splitBy j fac = map fst . tail $ scanl (\(d,m) i -> m `divMod` i) (0,j) fac

-- | Normalize index number as 0 based (minimum = 0)
index0base :: Index -> Int -> Int
index0base (_,s,_) j = j-s 

index0baseAll :: [Index] -> [Int] -> [Int]
index0baseAll = zipWith index0base

renormalizeIndex :: Index -> Int -> Int
renormalizeIndex (_,s,_) j = s+j

-- | From tuple index to flattened index with respect to a given index scheme.
flatIndex :: [Index] -> [Int] -> Int
flatIndex is pts = (index0baseAll is pts) `flattenBy` (indexFlatteningFactors is)

sizeIndex :: [Index] -> Int
sizeIndex is@(i:_) = indexSize i * head (indexFlatteningFactors is)

-- | From flattened index to tuple index with respect to a given index schema.
splitIndex :: [Index] -> Int -> [Int]
splitIndex is j = zipWith renormalizeIndex is (j `splitBy` indexFlatteningFactors is)


data Disjoint a = L a | R (Disjoint a)
                deriving Functor

deriving instance (Show a) => Show (Disjoint a)


partNth :: Int -> a -> Disjoint a
partNth 0 v = L v
partNth n v = R (partNth (n-1) v)

-- | flattening index for disjoint sum
flatIndexDisjoint :: [[Index]] -> Disjoint [Int] -> Int
flatIndexDisjoint (is:iss) (L j) = flatIndex is j
flatIndexDisjoint (is:iss) (R d) = sizeIndex is + flatIndexDisjoint iss d


-- | splitting index for disjoint sum
splitIndexDisjoint :: [[Index]] -> Int -> Disjoint [Int]
splitIndexDisjoint (is:iss) j
    | j < m     = L (splitIndex is j)
    | otherwise = R (splitIndexDisjoint iss (j-m))
  where m = sizeIndex is
