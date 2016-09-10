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


sum'_ :: (HasTrie a, ?expHash :: Exp a  :->: Hash) => [Index] -> MExp a -> MExp a
sum'_ is em@(MExp e1 m1 i1) =
  let h1 = untrie ?expHash e1
      i = i1 `HS.difference` HS.fromList is
      e = Sum is h1
      m = HM.insert h1 em m1
  in case e1 of
       Mul hs ds ->
         let es = map (flip justLookup m1) hs
             iss = map (map indexName . HS.toList . mexpIdx) es
             -- is' = map indexName is
             -- ds' = mapMaybe (\case Delta j k -> Just (indexName j,indexName k); CDelta _ _ _ -> Nothing)  ds
             
             test = get1Rule (head is) ds -- concatMap prettifyRule . concatMap (\x-> mkRule4Var x ds) $ is'
             
         in trace (show test) (MExp e m i )

       _ -> MExp e m i

 
data Rule = FromTo Index Index deriving Show

prettifyRule :: Rule -> String
prettifyRule (FromTo i j) = "(" ++ indexName i ++ "->" ++ indexName j ++ ")"


mkRule :: Index -> KDelta -> Maybe Rule
mkRule x (Delta y z) | x==y     = Just (FromTo x z)
                     | x==z     = Just (FromTo x y)
                     | otherwise = Nothing
--   where (y',z') = (indexName y, indexName z)
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

{- 
replace :: Rule -> MExp a -> MExp a
replace (FromTo i j) me@(MExp e m is)
  | i `HS.member` is = MExp e' m' ((HS.insert j . HS.delete i) is)
  | otherwise        = me
-}

{- 
| x==y'     = Just (FromTo x z' d)
                       | x==z'     = Just (FromTo x y' d)
                       | otherwise = Nothing
  where (y',z') = (indexName y, indexName z)
mkRule _ d = Nothing
-}
                             



  --msum (map (mkRule x) ds)
{-   <|> 
  let (d1s,d2s) = break isJust (map (mkRule x) ds)
                in case d2s of
                     []       -> (Nothing,ds)
                     (d':d's) -> (mkRule x d',ds++d's)
-}


{- 
-- mkRule4Var :: IndexSymbol -> [(IndexSymbol,IndexSymbol)] -> ([(IndexSymbol,IndexSymbol)])
mkRule4Var :: IndexSymbol -> [KDelta] -> [Rule]
mkRule4Var x ys = rights (map (mkRule x) ys) -- partitionEithers (map (mkRule x) ys)

-}


{-                      
mkRules :: [IndexSymbol] -> [(IndexSymbol,IndexSymbol)] -> (Maybe Rule,[(IndexSymbol,IndexSymbol)])
mkRules xs ds =
  where  

-}
