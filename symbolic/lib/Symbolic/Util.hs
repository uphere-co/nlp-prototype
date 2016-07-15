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
index0base :: [Index] -> [Int] -> [Int]
index0base = zipWith (\(_,s,_) j -> j-s) 

renormalizeIndex :: Index -> Int -> Int
renormalizeIndex (_,s,_) j = s+j

-- | From tuple index to flattened index with respect to a given index scheme.
flatIndex :: [Index] -> [Int] -> Int
flatIndex is pts = (index0base is pts) `flattenBy` (indexFlatteningFactors is)

  -- sum $ zipWith (\j f -> if f == 1 then j else j*f)
                   --         (index0base is pts) (indexFlatteningFactors is)

sizeIndex :: [Index] -> Int
sizeIndex is@(i:_) = indexSize i * head (indexFlatteningFactors is)

-- | From flattened index to tuple index with respect to a given index schema.
splitIndex :: [Index] -> Int -> [Int]
splitIndex is j = zipWith renormalizeIndex is (j `splitBy` indexFlatteningFactors is)


data Disjoint a = L a | R (Disjoint a)

deriving instance (Show a) => Show (Disjoint a)

-- | flattening index for disjoint sum
flatIndex4DisjointSum :: [[Index]] -> Disjoint [Int] -> Int
flatIndex4DisjointSum (is:iss) (L j) = flatIndex is j
flatIndex4DisjointSum (is:iss) (R d) =
  sizeIndex is + flatIndex4DisjointSum iss d


-- | splitting index for disjoint sum
splitIndex4DisjointSum :: [[Index]] -> Int -> Disjoint [Int]
splitIndex4DisjointSum (is:iss) j
    | j < m     = L (splitIndex is j)
    | otherwise = R (splitIndex4DisjointSum iss (j-m))
  where m = sizeIndex is
