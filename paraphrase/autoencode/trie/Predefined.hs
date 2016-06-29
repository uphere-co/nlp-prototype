{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Predefined where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.MemoTrie
--
import           Type
--

var :: String -> MExp
var s = MExp (Var (Simple s)) HM.empty HS.empty

ivar :: String -> Index -> MExp
ivar x i = MExp (Var (Indexed x i)) HM.empty HS.empty

x :: MExp
x = var "x"

y :: MExp
y = var "y"

x_ :: Index -> MExp
x_ i = ivar "x" i

y_ :: Index -> MExp
y_ i = ivar "y" i

one :: MExp 
one = MExp One HM.empty HS.empty

zero :: MExp
zero = MExp Zero HM.empty HS.empty

val :: Int -> MExp
val n = MExp (Val n) HM.empty HS.empty

delta j k = MExp (Delta j k) HM.empty HS.empty

biop :: (?expHash :: Exp :->: Hash) => Symbol -> MExp -> MExp -> MExp
biop sym em1@(MExp e1 m1 _) em2@(MExp e2 m2 _) =
  let h1 = untrie ?expHash e1
      h2 = untrie ?expHash e2
      e = Fun2 sym h1 h2
      m = (HM.insert h1 em1 . HM.insert h2 em2) (m1 `HM.union` m2)
  in MExp e m HS.empty

add :: (?expHash :: Exp :->: Hash) => MExp -> MExp -> MExp
add = biop (Simple "+")

mul :: (?expHash :: Exp :->: Hash) => MExp -> MExp -> MExp
mul = biop (Simple "*")

square :: (?expHash :: Exp :->: Hash) => MExp -> MExp
square e = mul e e 

power :: (?expHash :: Exp :->: Hash) => Int -> MExp -> MExp
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = one
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = square (power (n `div` 2) e) `mul` e

