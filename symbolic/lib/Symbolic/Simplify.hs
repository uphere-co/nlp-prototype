{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Simplify where

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
             is' = map indexName is
             -- ds' = mapMaybe (\case Delta j k -> Just (indexName j,indexName k); CDelta _ _ _ -> Nothing)  ds
             
             test = concatMap prettifyRule . concatMap (\x-> mkRule4Var x ds) $ is'
             
         in trace (show is' ++":"++ show iss ++ ":" ++show ds++":"++test) (MExp e m i )

       _ -> MExp e m i

 
data Rule = FromTo IndexSymbol IndexSymbol KDelta deriving Show

mkRule :: IndexSymbol -> KDelta -> Either KDelta Rule
mkRule x d@(Delta y z) | x==y'     = Right (FromTo x z' d)
                       | x==z'     = Right (FromTo x y' d)
                       | otherwise = Left d
  where (y',z') = (indexName y, indexName z)
mkRule _ d = Left d


-- mkRule4Var :: IndexSymbol -> [(IndexSymbol,IndexSymbol)] -> ([(IndexSymbol,IndexSymbol)])
mkRule4Var :: IndexSymbol -> [KDelta] -> [Rule]
mkRule4Var x ys = rights (map (mkRule x) ys) -- partitionEithers (map (mkRule x) ys)

prettifyRule :: Rule -> String
prettifyRule (FromTo i j _) = "(" ++ i ++ "->" ++ j ++ ")"



{-                      
mkRules :: [IndexSymbol] -> [(IndexSymbol,IndexSymbol)] -> (Maybe Rule,[(IndexSymbol,IndexSymbol)])
mkRules xs ds =
  where  

-}
