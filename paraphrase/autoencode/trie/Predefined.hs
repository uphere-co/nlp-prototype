{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Predefined where

import           Control.Arrow
import           Control.Lens              (over, _1)
import           Control.Monad.Trans.State
import           Data.Bits                 (xor)
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Text.Printf
--
import           Type
--
import Debug.Trace



x = ExpMap (Var "x") HM.empty

y = ExpMap (Var "y") HM.empty

one = ExpMap One HM.empty
zero = ExpMap Zero HM.empty

val n = ExpMap (Val n) HM.empty

biop :: (?expHash :: Exp :->: Hash) => Symbol -> ExpMap -> ExpMap -> ExpMap
biop sym em1@(ExpMap e1 m1) em2@(ExpMap e2 m2) =
  let h1 = untrie ?expHash e1
      h2 = untrie ?expHash e2
      e = Fun2 sym h1 h2
      m = (HM.insert h1 em1 . HM.insert h2 em2) (m1 `HM.union` m2)
  in ExpMap e m

add :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap -> ExpMap
add = biop "+"

mul :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap -> ExpMap
mul = biop "*"

square :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap
square e = mul e e 

power :: (?expHash :: Exp :->: Hash) => Int -> ExpMap -> ExpMap
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = one
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = square (power (n `div` 2) e) `mul` e

