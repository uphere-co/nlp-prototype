{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Simplify where

import           Data.MemoTrie
import           Data.Monoid
--
import           Symbolic.Predefined
import           Symbolic.Type
--

add' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
add' es = let es' = (filter (not . isZero . mexpExp) . concatMap (flatten1 argsAdd)) es
          in case es' of
               []   -> zero
               e:[] -> e
               _    -> add es'

mul' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
mul' es = let es' = (filter (not . isOne . mexpExp) . concatMap (flatten1 argsMul)) es
          in if (getAny (foldMap (Any . isZero . mexpExp) es))
               then zero
               else case es' of
                      []   -> one
                      e:[] -> e
                      _    -> mul es'

flatten1 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (MExp a -> Maybe [MExp a]) -> MExp a -> [MExp a]
flatten1 f e = maybe [e] id (f e)

argsAdd :: MExp a -> Maybe [MExp a]
argsAdd (MExp (Add hs) m _) = Just (map (flip justLookup m) hs)
argsAdd _                   = Nothing

argsMul :: MExp a -> Maybe [MExp a]
argsMul (MExp (Mul hs ds) m _) = Just (map (flip justLookup m) hs)
argsMul _                      = Nothing
