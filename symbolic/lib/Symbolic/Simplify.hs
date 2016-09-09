{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Simplify where

import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Data.Monoid
--
import           Symbolic.Predefined
import           Symbolic.Type
--

add' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
add' es = let es' = (filter (not . isZero . mexpExp) . concatMap liftAdd) es
          in case es' of
               []   -> zero
               e:[] -> e
               _    -> add es'

mul' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
mul' es = let concatTuple xs = (concatMap fst xs, concatMap snd xs)
              (es',ds') = (concatTuple . map liftMul) es
              es'' = filter (not . isOne . mexpExp) es'
          in if (getAny (foldMap (Any . isZero . mexpExp) es''))
               then zero
               else case (es'',ds') of
                      ([],[])   -> one
                      (e:[],[]) -> e
                      _         ->
                        let (hs,m,is) = findTriple es''
                            is' = HS.fromList (concatMap deltaIndex ds')
                        in MExp (Mul hs ds') m (HS.union is is') 


liftAdd :: MExp a -> [MExp a]
liftAdd x@(MExp (Add hs) m _)    = map (flip justLookup m) hs
liftAdd x@(_)                    = [x] 

liftMul :: MExp a -> ([MExp a],[KDelta])
liftMul x@(MExp (Mul hs ds) m _) = (map (flip justLookup m) hs,ds)
liftMul x@(_)                    = ([x],[])

{- 
flatten1 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (MExp a -> Maybe [MExp a]) -> MExp a -> [MExp a]
flatten1 f e = maybe [e] id (f e)

argsAdd :: MExp a -> Maybe [MExp a]
argsAdd (MExp (Add hs) m _) = Just (map (flip justLookup m) hs)
argsAdd _                   = Nothing

argsMul :: MExp a -> Maybe [MExp a]
argsMul (MExp (Mul hs ds) m _) = Just (map (flip justLookup m) hs)
argsMul _                      = Nothing
-}
