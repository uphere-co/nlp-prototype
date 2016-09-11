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

repIndex :: Rule -> Index -> Index
repIndex (FromTo i j) k = if i == k then j else k

repDelta :: Rule -> KDelta -> KDelta
repDelta r (Delta x y)      = Delta (repIndex r x) (repIndex r y)
-- repDelta r (CDelta i iss p) = CDelta (repIndex r i) (fmap (fmap repIndex) iss) p   -- think about this more.

repDelta (FromTo i j) d = d -- this should be implemented 


repVar :: Rule -> Variable -> Variable
repVar r (V s is) = V s (map (repIndex r) is)

replace :: Rule -> MExp a -> MExp a
replace r@(FromTo i j) me@(MExp e m is)
  | i `HS.member` is =
      let is' = (HS.insert j . HS.delete i) is
          me' = MExp e m is'
      in case e of
           Zero        -> zero
           One         -> one
           Val n       -> val n
           Var x       -> noStructure (Var (repVar r x))
           Add hs      -> me'
           Mul hs ds   -> me'
           Fun s hs    -> me'
           Sum is h    -> me'
           Concat i hs -> me'
  | otherwise = me

-- eliminate

--     MExp e' m' ()

mkNewSum is_rem (es,ds) = 
  let nt = mul' (es++map kdelta ds)
  in if null is_rem then nt else sum_ is_rem nt


      -- ([h],m,i) = findTriple [nt] 
      -- nt_h1 = getMHash nt
      -- sume_is = tail is
      -- m' = HM.insert nt_h1 nt (mexpMap nt)


      -- iss = map (map indexName . HS.toList . mexpIdx) es
      -- is' = map indexName is

      -- (hs',m,is') = findTriple es'

      -- mnt = MExp  nt m is'
      -- hs' = map getMHash es'
      
      --  m' = HM.insert h1 em m1
      
      -- newterm = Mul

  -- in trace (prettifyRule rule) s
    

sum'_ :: (HasTrie a, ?expHash :: Exp a  :->: Hash) => [Index] -> MExp a -> MExp a
sum'_ is (MExp (Mul hs ds) m i) =
  let es = map (flip justLookup m) hs
      i' = head is
      Just (rule,ds') = get1Rule i' ds
      es' = map (replace rule) es
  in mkNewSum (tail is) (es',ds')
sum'_ is em = sum_ is em

