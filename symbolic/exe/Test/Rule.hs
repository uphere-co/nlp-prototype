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

rule1 :: IO ()
rule1 = do
  let ?expHash = trie hash
  let idxj, idxn :: Index
      idxj = ("j",1,2)
      idxn = ("n",1,2)
      r = sum'_ [idxn] (mul' [one,delta idxn idxj])  
  printf "r = %s\n"  ((prettyPrint . exp2RExp) (r ::  MExp Int) :: String)




rule2 :: IO ()
rule2 = do
  let idxi, idxj, idxm, idxn, idxk :: Index
      idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxk = ("k",1,2)
      idxm = ("m",1,2)
      idxn = ("n",1,2)
  let ?expHash = trie hash
  let e1 = sum'_ [idxn,idxm] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi ,idxk], delta idxi idxm, delta idxj idxn])
      e2 = sum'_ [idxm,idxn] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi, idxk], delta idxi idxn, delta idxj idxm])
      r = add' [e1,e2]
  let ne1 = sum_ [idxn,idxm] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi ,idxk], delta idxi idxm, delta idxj idxn])
      ne2 = sum_ [idxm,idxn] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi, idxk], delta idxi idxn, delta idxj idxm])
      nr = add [ne1,ne2]

  printf "nr = %s\n"  ((prettyPrint . exp2RExp) (nr ::  MExp Int) :: String)

        
  printf "r = %s\n"  ((prettyPrint . exp2RExp) (r ::  MExp Int) :: String)


