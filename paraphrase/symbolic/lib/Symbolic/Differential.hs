{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Differential where

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

suffix' :: Symbol -> Symbol
suffix' (Simple s) = Simple (s <> "'")

suffix_1 :: Symbol -> Symbol
suffix_1 (Simple s) = Simple (s <> "_1")

suffix_2 :: Symbol -> Symbol
suffix_2 (Simple s) = Simple (s  <> "_2")
                    
diff'
  :: (?expHash :: Exp :->: Hash)
  => HashMap Hash MExp
  -> ((Symbol,Exp) :->: MExp)
  -> (Symbol,Exp) -> MExp
diff' m t (s,e) =
  case e of
    Zero         -> zero 
    One          -> zero
    Val _        -> zero
    Var s'       -> dvar s s' 
    Fun1 f h1    -> let MExp e1 _ _ = justLookup h1 m
                    in MExp (Fun1 (suffix' f) h1) m HS.empty `mul'` untrie t (s,e1)
    Fun2 f h1 h2 -> let MExp e1 _ _ = justLookup h1 m
                        MExp e2 _ _ = justLookup h2 m
                    in (simplify2 m f Pos1 h1 h2 `mul'` untrie t (s,e1)) `add'`
                         (simplify2 m f Pos2 h1 h2 `mul'` untrie t (s,e2)) 
    Sum is h1    -> let MExp e1 _ _ = justLookup h1 m
                    in sum_ is (untrie t (s,e1))


dvar (Simple s)    (Simple s')   = if s == s' then one else zero
dvar (Simple s)    _             = zero
dvar _             (Simple s')   = zero
dvar (Indexed x j) (Indexed y k) = if x == y then delta j k else zero

data Pos = Pos1 | Pos2 


simplify2 :: HashMap Hash MExp -> Symbol -> Pos -> Hash -> Hash -> MExp
simplify2 m f pos h1 h2
  | showSym f == "+" = one
  | showSym f == "*" = case pos of
                         Pos1 -> justLookup h2 m
                         Pos2 -> justLookup h1 m
  | otherwise        = case pos of
                         Pos1 -> MExp (Fun2 (suffix_1 f) h1 h2) m HS.empty
                         Pos2 -> MExp (Fun2 (suffix_2 f) h1 h2) m HS.empty

add' :: (?expHash :: Exp :->: Hash) => MExp -> MExp -> MExp
add' e1                 (mexpExp -> Zero)  = e1
add' (mexpExp -> Zero)  e2                 = e2
add' (mexpExp -> One)   (mexpExp -> One)   = val 2
add' (mexpExp -> Val m) (mexpExp -> One)   = val (m+1)
add' (mexpExp -> One)   (mexpExp -> Val m) = val (m+1)
add' (mexpExp -> Val m) (mexpExp -> Val n) = val (m+n)
add' e1                 e2                 = e1 `add` e2

mul' :: (?expHash :: Exp :->: Hash) => MExp -> MExp -> MExp
mul' _                   (mexpExp -> Zero) = zero
mul' (mexpExp -> Zero)   _                 = zero
mul' e1                  (mexpExp -> One)  = e1
mul' (mexpExp -> One)    e2                = e2
mul' e1                  e2                = e1 `mul` e2 


sdiff :: (?expHash :: Exp :->: Hash) => Symbol -> MExp -> MExp
sdiff s (MExp e m _) = let diff = fix (diff' m . trie) in diff (s,e)

