{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.MemoTrie
--
import           Symbolic.Differential
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--

expfib' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (Int :->: MExp a) -> Int -> MExp a
expfib' _ 0 = varx
expfib' _ 1 = vary
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add [e1, e2]

expfib :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Int -> MExp a
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib

testfib :: IO ()
testfib = do
  let ?expHash = trie hash    
  let n = 5
      lexp1 = expfib n :: MExp Int
      -- lexp2 = dexpfib (V (mkSym "x") [],n)
  prettyPrintR $ lexp1
  -- prettyPrintR $ lexp2    


main = testfib
