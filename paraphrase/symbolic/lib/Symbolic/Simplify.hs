{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Simplify where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Data.Monoid
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


mul' :: (HasTrie a, ?expHash :: Exp a :->: Hash) => MExp a -> MExp a -> MExp a
mul' _                   (mexpExp -> Zero) = zero
mul' (mexpExp -> Zero)   _                 = zero
mul' e1                  (mexpExp -> One)  = e1
mul' (mexpExp -> One)    e2                = e2
mul' e1                  e2                = e1 `mul` e2 

-}

add' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
add' es = let es' = (filter (not . isZero . mexpExp) . concatMap (flatten1 argsAdd)) es
          in if null es' then zero else add es'

mul' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
mul' es = let es' = (filter (not . isOne . mexpExp) . concatMap (flatten1 argsMul)) es
          in if | null es'                                     -> one
                | getAny (foldMap (Any . isZero . mexpExp) es) -> zero
                | otherwise                                    -> mul es'

flatten1 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (MExp a -> Maybe [MExp a]) -> MExp a -> [MExp a]
flatten1 f e = maybe [e] id (f e)


argsAdd (MExp (Add hs) m i) = Just (map (flip justLookup m) hs)
argsAdd _                   = Nothing

argsMul (MExp (Mul hs) m i) = Just (map (flip justLookup m) hs)
argsMul _                   = Nothing
