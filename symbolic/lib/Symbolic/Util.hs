module Symbolic.Util where

import Symbolic.Type

indexFlatteningFactors :: [Index] -> [Int]
indexFlatteningFactors is = scanr (*) 1 (tail (map (\(_,s,e) -> e-s+1)  is ))

index0base :: [Index] -> [Int] -> [Int]
index0base = zipWith (\(_,s,_) j -> j-s) 

combinedIndex :: [Index] -> [Int] -> Int
combinedIndex is pts = sum $ zipWith (\j f -> if f == 1 then j else j*f)
                               (index0base is pts) (indexFlatteningFactors is)

splitIndex :: [Index] -> Int -> [Int]
splitIndex is j = map fst . tail $ scanl (\(d,m) i -> m `divMod` i) (0,j) factors
  where factors = indexFlatteningFactors is
