{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Simplify where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.State
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Data.Monoid               ((<>))
import           Text.Printf
--
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--

{- 
simplify2 :: HashMap Hash (MExp a) -> String -> Pos -> Hash -> Hash -> MExp a
simplify2 m f pos h1 h2
  | f == "+"  = one
  | f == "*"  = case pos of
                  Pos1 -> justLookup h2 m
                  Pos2 -> justLookup h1 m
  | otherwise = case pos of
                  Pos1 -> MExp (Fun2 (suffix_1 f) h1 h2) m HS.empty
                  Pos2 -> MExp (Fun2 (suffix_2 f) h1 h2) m HS.empty

add' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> MExp a -> MExp a
add' e1                 (mexpExp -> Zero)  = e1
add' (mexpExp -> Zero)  e2                 = e2
add' (mexpExp -> One)   (mexpExp -> One)   = val 2
add' (mexpExp -> Val m) (mexpExp -> One)   = val (m+1)
add' (mexpExp -> One)   (mexpExp -> Val m) = val (m+1)
add' (mexpExp -> Val m) (mexpExp -> Val n) = val (m+n)
add' e1                 e2                 = e1 `add` e2

mul' :: (HasTrie a, ?expHash :: Exp a :->: Hash) => MExp a -> MExp a -> MExp a
mul' _                   (mexpExp -> Zero) = zero
mul' (mexpExp -> Zero)   _                 = zero
mul' e1                  (mexpExp -> One)  = e1
mul' (mexpExp -> One)    e2                = e2
mul' e1                  e2                = e1 `mul` e2 

-}
