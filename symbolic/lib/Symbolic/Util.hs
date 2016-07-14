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

maxFlatIndex :: [Index] -> Int
maxFlatIndex is@(i:_) = indexSize i * head (indexFlatteningFactors is)

-- | From flattened index to tuple index with respect to a given index schema.
splitIndex :: [Index] -> Int -> [Int]
splitIndex is j = j `splitBy` indexFlatteningFactors is


flatIndex4DisjointSum :: [[Index]] -> [[Int]] -> Int
flatIndex4DisjointSum iss ptss = eachFI `flattenBy` factors
  where factors = scanr (*) 1 (tail (map maxFlatIndex iss))
        eachFI = zipWith flatIndex iss ptss


splitIndex4DisjointSum :: [[Index]] -> Int -> [[Int]]
splitIndex4DisjointSum iss j = zipWith splitNRenorm iss i's  
  where factors = scanr (*) 1 (tail (map maxFlatIndex iss))
        i's = j `splitBy` factors
        splitNRenorm is i' = zipWith renormalizeIndex is (splitIndex is i')
