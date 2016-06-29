{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Predefined where

import qualified Data.HashMap.Strict as HM
import           Data.MemoTrie
--
import           Type
--

var :: String -> ExpMap
var s = ExpMap (Var (Simple s)) HM.empty

ivar :: String -> String -> ExpMap
ivar x i = ExpMap (Var (Indexed x i)) HM.empty

x :: ExpMap
x = var "x" -- ExpMap (Var (Simple "x")) HM.empty

y :: ExpMap
y = var "y" -- ExpMap (Var (Simple "y")) HM.empty

x_ :: String -> ExpMap
x_ i = ivar "x" i

y_ :: String -> ExpMap
y_ i = ivar "y" i

one :: ExpMap 
one = ExpMap One HM.empty

zero :: ExpMap
zero = ExpMap Zero HM.empty

val :: Int -> ExpMap
val n = ExpMap (Val n) HM.empty

biop :: (?expHash :: Exp :->: Hash) => Symbol -> ExpMap -> ExpMap -> ExpMap
biop sym em1@(ExpMap e1 m1) em2@(ExpMap e2 m2) =
  let h1 = untrie ?expHash e1
      h2 = untrie ?expHash e2
      e = Fun2 sym h1 h2
      m = (HM.insert h1 em1 . HM.insert h2 em2) (m1 `HM.union` m2)
  in ExpMap e m

add :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap -> ExpMap
add = biop (Simple "+")

mul :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap -> ExpMap
mul = biop (Simple "*")

square :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap
square e = mul e e 

power :: (?expHash :: Exp :->: Hash) => Int -> ExpMap -> ExpMap
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = one
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = square (power (n `div` 2) e) `mul` e

