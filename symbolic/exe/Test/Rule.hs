{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Rule where

import           Data.Foldable             (forM_)
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.MemoTrie
import qualified Data.Vector.Storable as VS
import           Text.Printf
--
-- import           Symbolic.CodeGen.C
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Simplify
import           Symbolic.Type
import           Symbolic.Util
--

showExp s e = printf "%s = %s\n" s ((prettyPrint . exp2RExp) e :: String)

rule1 :: IO ()
rule1 = do
  let ?expHash = trie hash
  let idxj, idxn :: Index
      idxj = ("j",1,2)
      idxn = ("n",1,2)
      original, changed :: MExp Int
      original = sum_ [idxn] (mul [one,delta idxn idxj])
      changed = sum'_ [idxn] (mul' [one,delta idxn idxj])
  showExp "original" original
  showExp "changed" changed

rule2 :: IO ()
rule2 = do
  let idxi, idxj, idxm, idxn, idxk :: Index
      idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxk = ("k",1,2)
      idxm = ("m",1,2)
      idxn = ("n",1,2)
  let ?expHash = trie hash
  let original, changed :: MExp Int
      ne1 = sum_ [idxn,idxm] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi ,idxk], delta idxi idxm, delta idxj idxn])
      ne2 = sum_ [idxm,idxn] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi, idxk], delta idxi idxn, delta idxj idxm])
      original = add [ne1,ne2]  
      e1 = sum'_ [idxn,idxm] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi ,idxk], delta idxi idxm, delta idxj idxn])
      e2 = sum'_ [idxm,idxn] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi, idxk], delta idxi idxn, delta idxj idxm])
      changed = add' [e1,e2]
  showExp "original" original
  showExp "changed" changed


