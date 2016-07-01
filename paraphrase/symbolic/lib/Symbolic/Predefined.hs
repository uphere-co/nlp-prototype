{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Symbolic.Predefined where

import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (difference)
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Data.Monoid               ((<>))
--
import           Symbolic.Type
--

var :: String -> MExp
var s = MExp (Var (Simple s)) HM.empty HS.empty

ivar :: String -> [Index] -> MExp
ivar x i = MExp (Var (Indexed x i)) HM.empty (HS.fromList i)

x :: MExp
x = var "x"

y :: MExp
y = var "y"

x_ :: [Index] -> MExp
x_ i = ivar "x" i

y_ :: [Index] -> MExp
y_ i = ivar "y" i

one :: MExp 
one = MExp One HM.empty HS.empty

zero :: MExp
zero = MExp Zero HM.empty HS.empty

val :: Double -> MExp
val n = MExp (Val n) HM.empty HS.empty

delta j k = MExp (Delta j k) HM.empty (HS.fromList [j,k])

biop :: (?expHash :: Exp :->: Hash) => String -> MExp -> MExp -> MExp
biop sym em1@(MExp e1 m1 i1) em2@(MExp e2 m2 i2) =
  let h1 = untrie ?expHash e1
      h2 = untrie ?expHash e2
      e = Fun2 sym h1 h2
      m = (HM.insert h1 em1 . HM.insert h2 em2) (m1 `HM.union` m2)
  in MExp e m (HS.union i1 i2)

add :: (?expHash :: Exp :->: Hash) => MExp -> MExp -> MExp
add = biop ("+")

mul :: (?expHash :: Exp :->: Hash) => MExp -> MExp -> MExp
mul = biop ("*")

sum_ :: (?expHash :: Exp :->: Hash) => [Index] -> MExp -> MExp
sum_ is em@(MExp e1 m1 i1) =
  let h1 = untrie ?expHash e1
      i = i1 `difference` HS.fromList is
      e = Sum is h1
      m = HM.insert h1 em m1
  in MExp e m i

square :: (?expHash :: Exp :->: Hash) => MExp -> MExp
square e = mul e e 

power :: (?expHash :: Exp :->: Hash) => Int -> MExp -> MExp
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = one
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = square (power (n `div` 2) e) `mul` e

suffix' :: String -> String
suffix' = (<> "'")

suffix_1 :: String -> String
suffix_1 = (<> "_1")

suffix_2 :: String -> String
suffix_2 = (<> "_2")

fun :: (?expHash :: Exp :->: Hash) => String -> MExp -> MExp
fun sym e@(MExp e1 m1 i1) =
  let h1 = untrie ?expHash e1
  in MExp (Fun1 sym h1) (HM.insert h1 e m1) i1 

tanh_ :: (?expHash :: Exp :->: Hash) => MExp -> MExp
tanh_ = fun "tanh" 
