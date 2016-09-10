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



delta_rule :: IO ()
delta_rule = do
  let idxi, idxj, idxm, idxn, idxk :: Index
      idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxk = ("k",1,2)
      idxm = ("m",1,2)
      idxn = ("n",1,2)
  let ?expHash = trie hash
  let e1 = sum'_ [idxm,idxn] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi ,idxk], delta idxi idxm, delta idxj idxn])
      e2 = sum'_ [idxm,idxn] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi, idxk], delta idxi idxn, delta idxj idxm])
      e3 = add' [e1,e2]
  printf "e3 = %s\n"  ((prettyPrint . exp2RExp) (e3 ::  MExp Int) :: String)

  -- digraph e3


