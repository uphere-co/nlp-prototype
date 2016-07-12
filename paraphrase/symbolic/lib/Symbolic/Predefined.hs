{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Symbolic.Predefined where

import           Control.Lens              (view,_1)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (difference)
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Data.Monoid               ((<>))
--
import           Symbolic.Type
--

var :: String -> MExp a
var s = MExp (Var (Simple s)) HM.empty HS.empty

ivar :: String -> [Index] -> MExp a
ivar x i = MExp (Var (Indexed x i)) HM.empty (HS.fromList i)

x :: MExp a
x = var "x"

y :: MExp a
y = var "y"

z :: MExp a
z = var "z"

x_ :: [Index] -> MExp a
x_ i = ivar "x" i

y_ :: [Index] -> MExp a
y_ i = ivar "y" i

z_ :: [Index] -> MExp a
z_ i = ivar "z" i

one :: MExp a
one = MExp One HM.empty HS.empty

zero :: MExp a 
zero = MExp Zero HM.empty HS.empty

val :: a -> MExp a
val n = MExp (Val n) HM.empty HS.empty

delta j k = MExp (Delta j k) HM.empty (HS.fromList [j,k])

varop :: (HasTrie a, ?expHash :: Exp a :->: Hash) => ([Hash] -> Exp a) -> [MExp a] -> MExp a
varop op es = let hes = map ((,) <$> untrie ?expHash . mexpExp <*> id) es
                  ms = map mexpMap es
                  is = map mexpIdx es
                  m' = foldl1 HM.union ms
                  i' = foldl1 HS.union is
                  m'' = foldr (uncurry HM.insert) m' hes
              in MExp (op (map fst hes)) m'' i'

add :: (HasTrie a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
add = varop Add

mul :: (HasTrie a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
mul = varop Mul

sum_ :: (HasTrie a, ?expHash :: Exp a  :->: Hash) => [Index] -> MExp a -> MExp a
sum_ is em@(MExp e1 m1 i1) =
  let h1 = untrie ?expHash e1
      i = i1 `difference` HS.fromList is
      e = Sum is h1
      m = HM.insert h1 em m1
  in MExp e m i

fun :: (HasTrie a, ?expHash :: Exp a :->: Hash) => String -> [MExp a] -> MExp a
fun sym = varop (Fun sym)

square :: (HasTrie a, ?expHash :: Exp a :->: Hash) => MExp a -> MExp a
square e = mul [e,e] 

power :: (HasTrie a, ?expHash :: Exp a :->: Hash) => Int -> MExp a -> MExp a
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = one
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = mul [square (power (n `div` 2) e), e]



suffix' :: String -> String
suffix' = (<> "'")

suffix_1 :: String -> String
suffix_1 = (<> "_1")

suffix_2 :: String -> String
suffix_2 = (<> "_2")

suffix_n :: Int -> String -> String
suffix_n n = (<> ("_" ++ show n))


{-
tanh_ :: (HasTrie a, ?expHash :: Exp a :->: Hash) => MExp a -> MExp a
tanh_ = fun "tanh"
-}
