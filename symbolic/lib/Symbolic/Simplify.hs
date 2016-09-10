{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Simplify where

import           Control.Applicative
import           Control.Monad
-- import           Control.Monad.Loops
import           Data.Either
import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.MemoTrie
import           Data.Monoid               ((<>),Any(..),getAny)
--
import           Symbolic.Predefined
import           Symbolic.Type
--
import           Debug.Trace

liftAdd :: MExp a -> [MExp a]
liftAdd x@(MExp (Add hs) m _)    = map (flip justLookup m) hs
liftAdd x@(_)                    = [x] 

liftMul :: MExp a -> ([MExp a],[KDelta])
liftMul x@(MExp (Mul hs ds) m _) = (map (flip justLookup m) hs,ds)
liftMul x@(_)                    = ([x],[])




add' :: (HasTrie a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
add' es = let es' = (filter (not . isZero . mexpExp) . concatMap liftAdd) es
          in case es' of
               []   -> zero
               e:[] -> e
               _    -> add es'

mul' :: (HasTrie a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
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


sum'_ :: (HasTrie a, ?expHash :: Exp a  :->: Hash) => [Index] -> MExp a -> MExp a
sum'_ is (MExp (Mul hs ds) m i) =
  let es = map (flip justLookup m) hs
      -- iss = map (map indexName . HS.toList . mexpIdx) es
      -- is' = map indexName is
      i' = head is
      Just (rule,ds') = get1Rule i' ds
      es' = map (replace rule) es
      -- (hs',m,is') = findTriple es'
      nt = mul' (es'++map kdelta ds') -- Mul hs' ds'
      -- mnt = MExp  nt m is'
      -- hs' = map getMHash es'
      
      --  m' = HM.insert h1 em m1
      
      -- newterm = Mul
  in trace (prettifyRule rule) nt -- (MExp newterm m i) -- (MExp r m (HS.delete i' i))
sum'_ is em@(MExp e1 m1 i1) = let h1 = untrie ?expHash e1
                                  i = i1 `HS.difference` HS.fromList is
                                  e = Sum is h1
                                  m = HM.insert h1 em m1
                              in MExp e m i

 
data Rule = FromTo Index Index deriving Show

prettifyRule :: Rule -> String
prettifyRule (FromTo i j) = "(" ++ indexName i ++ "->" ++ indexName j ++ ")"


mkRule :: Index -> KDelta -> Maybe Rule
mkRule x (Delta y z) | x==y     = Just (FromTo x z)
                     | x==z     = Just (FromTo x y)
                     | otherwise = Nothing
mkRule _ _ = Nothing   -- this should be implemented. 
 
get1Rule :: Index -> [KDelta] -> Maybe (Rule,[KDelta]) -- Maybe (Rule,[KDelta])
get1Rule x ds = go [] ds
  where
    go ds' (d:ds) = (mkRule x d >>= \r -> return (r,map (repDelta r) (ds'++ds))) <|> (go (ds'++[d]) ds)
    go _ []       = Nothing

repDelta :: Rule -> KDelta -> KDelta
repDelta (FromTo i j) d@(Delta x y)
  = Delta (if i == x then j else x) (if i == y then j else y) 
repDelta (FromTo i j) d = d -- this should be implemented 





replace :: Rule -> MExp a -> MExp a
replace (FromTo i j) me@(MExp e m is)
  | i `HS.member` is =
      let is' = (HS.insert j . HS.delete i) is
          me' = MExp e m is'
      in case e of
           Zero        -> zero
           One         -> one
           Val n       -> me'
           Var x       -> me'
           Add hs      -> me'
           Mul hs ds   -> me'
           Fun s hs    -> me'
           Sum is h    -> me'
           Concat i hs -> me'
  | otherwise = me


--     MExp e' m' ()
                             

