{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Eval where

import           Control.Lens               (view, _1)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashSet         as HS
import           Data.List                  (foldl')
import           Data.MemoTrie
import           Data.Vector.Storable       (Storable,(!))
import qualified Data.Vector.Storable as VS
--
import           Symbolic.Type
import           Symbolic.Util
--

evalVar :: (Num a, VS.Storable a) => Args a -> IdxPoint -> Symbol -> a
evalVar args _  (Simple s) = (justLookup s . varSimple) args
evalVar args ip (Indexed s is) = let i's = map (flip justLookupL ip . view _1) is
                                     ival = justLookup s (varIndexed args)
                                 in valStore ival ! ivalFlatIndex ival i's

evalDelta :: (Num a) => [(IndexSymbol,Int)] -> IndexSymbol -> IndexSymbol -> a
evalDelta ip i j = let i' = justLookupL i ip
                       j' = justLookupL j ip
                   in if i' == j' then 1 else 0 


eval :: ( Num a, Storable a, HasTrie a
        , ?expHash :: Exp a :->: Hash
        , ?functionMap :: FunctionMap a
        ) =>
        HashMap Hash (MExp a)
     -> (Args a,IdxPoint,Exp a)
     -> a
eval _ (_,_,Zero)         = 0
eval _ (_,_,One)          = 1
eval _ (_,_,Val n)        = n
eval _ (_,ip,Delta (i,_,_) (j,_,_)) = evalDelta ip i j 
eval _ (args,ip,Var v)    = evalVar args ip v
eval m (args,ip,Mul hs)   = let es = map (mexpExp . flip justLookup m) hs
                            in foldl' (*) 1 (map (\e->eval m (args,ip,e)) es)
eval m (args,ip,Add hs)   = let es = map (mexpExp . flip justLookup m) hs
                            in foldl' (+) 0 (map (\e->eval m (args,ip,e)) es)
eval m (args,ip,Fun s hs) = let es = map (mexpExp . flip justLookup m) hs
                                f = justLookup s ?functionMap      
                            in f (map (\e->eval m (args,ip,e)) es)
eval m (args,ip,Sum is h) = let idx1lst (idx,start,end) = [(idx,v)| v <- [start..end]]
                                sumip = traverse idx1lst is                
                                ip' = map (ip ++) sumip
                                e = justLookup h m
                            in (foldl' (+) 0 . map (\i -> eval m (args,i,mexpExp e))) ip'
eval m (args,ip,Concat idx hs) = select es di
  where es = map (flip justLookup m) hs
        i' = index0base idx (justLookupL (view _1 idx) ip)
        dis = map (HS.toList . mexpIdx) es 
        di = splitIndexDisjoint dis i'
        -- di' = fmap (zipWith (\(i,_,_) v -> (i,v)) dis) di
        select (x:xs) (L i)
          = let i' = zipWith (\(k,_,_) v -> (k,v)) (HS.toList (mexpIdx x)) i
            in eval m (args,i',mexpExp x)
        select (x:xs) (R d) = select xs d
-- findElement args (e:es) (L i) = eval m (args,i,mexpExp e)  

seval :: ( Storable a, HasTrie a, Num a
         , ?expHash :: Exp a :->: Hash
         , ?functionMap :: FunctionMap a
         ) =>
         Args a -> IdxPoint -> MExp a -> a
seval a ip e = eval (mexpMap e) (a,ip,mexpExp e)
