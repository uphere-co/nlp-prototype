{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Fib where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.State
import           Data.Foldable             (forM_)
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.List                 (lookup,foldl')
import           Data.Maybe                (fromJust)
import           Data.MemoTrie
import           Data.Monoid               ((<>))
import           Data.Vector.Storable       (Vector(..),Storable(..),(!))
import qualified Data.Vector.Storable as VS
import           Text.Printf
--
import           Symbolic.CodeGen.C
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Simplify
import           Symbolic.Type
import           Symbolic.Util


expfib' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (Int :->: MExp a) -> Int -> MExp a
expfib' _ 0 = x -- x_ ["i"]
expfib' _ 1 = y -- y_ ["i"]
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add [e1, e2]

expfib :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Int -> MExp a
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib

dexpfib' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => 
            (Int :->: MExp a, (Symbol,Exp a) :->: MExp a)
         -> (Symbol,Int) -> MExp a
dexpfib' (tfib,tdiff) (s,n) = let MExp e m _ = untrie tfib n in diff' m tdiff (s,e)

dexpfib :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (Symbol,Int) -> MExp a
dexpfib (s,n) = let tfib = trie ffib
                    ffib = expfib' tfib
                    MExp _ m _ = untrie tfib n
                    tdiff = trie (diff' m tdiff)
                    f = dexpfib' (tfib,tdiff) 
                in f (s,n)

{- 
eval_fib :: (HasTrie a, Num a, Floating a, ?expHash :: Exp a :->: Hash) => Args a -> IdxPoint -> Int -> EExp a
eval_fib a ip n = let tfib = trie ffib
                      ffib = expfib' tfib
                      e = mexpExp (ffib n)
                      m = mexpMap (ffib n)
                      feval = eval m teval
                      teval = trie feval 
                  in untrie teval (a,ip,e)
-}

testfib :: IO ()
testfib = do
  let ?expHash = trie hash    
  -- let lexp1 = expfib 10
  let n = 5
      lexp1 = expfib n :: MExp Int
      lexp2 = dexpfib (Simple "x",n) -- (Indexed "x" ["j"],n)
  prettyPrintR $ lexp1
  prettyPrintR $ lexp2    
  -- (printf "lexp2: %x\n" . untrie ?expHash . mexpExp) lexp2

{-
test7 :: IO ()
test7 = do
  let ?expHash = trie hash :: Exp Double :->: Hash
  let n = 100
      -- lexp1 = expfib n
      -- lexp2 = dexpfib (Indexed "x" ["j"],n)
  -- prettyPrintR lexp1
  let args = [(Simple "x",1),(Simple "y",1 :: Double)]
  -- let args' = [(Indexed "x" 1,1),(Indexed "y" 1,1 :: Double)]  
  prettyPrintE $ seval args [] exp3 -- eval_fib args n -- eval args lexp1
-}


