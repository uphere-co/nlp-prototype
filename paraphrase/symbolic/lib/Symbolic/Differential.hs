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
import           Symbolic.Simplify
import           Symbolic.Type
--

                   
diff'
  :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash)
  => HashMap Hash (MExp a)
  -> ((Symbol,Exp a) :->: MExp a)
  -> (Symbol,Exp a) -> MExp a
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


dvar :: (HasTrie a, ?expHash :: Exp a :->: Hash) => Symbol -> Symbol -> MExp a
dvar (Simple s)    (Simple s')   = if s == s' then one else zero
dvar (Simple s)    _             = zero
dvar _             (Simple s')   = zero
dvar (Indexed x j) (Indexed y k)
  | x == y && length j == length k = let djk = zipWith delta j k
                                     in foldr1 mul djk
  | otherwise = zero


sdiff :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Symbol -> MExp a -> MExp a
sdiff s (MExp e m _) = let diff = fix (diff' m . trie) in diff (s,e)

