{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Eval where

import           Control.Lens              (view, _1)
import           Data.HashMap.Strict       (HashMap)
import           Data.List                 (foldl')
import           Data.MemoTrie
import           Data.Vector.Storable       (Storable,(!))
import qualified Data.Vector.Storable as VS
--
import           Symbolic.Type
--

eval :: ( Num a, Storable a, HasTrie a
        , ?expHash :: Exp a :->: Hash
        , ?functionMap :: FunctionMap a
        ) =>
        HashMap Hash (MExp a)
     -> (Args a,IdxPoint,Exp a)
     -> a
eval _ (_,_,Zero) = 0
eval _ (_,_,One)  = 1
eval _ (_,_,Val n) = n
eval _ (_,ip,Delta i j) = evalDelta ip i j 
eval m (args,ip,Var v) = evalVar args ip v
eval m (args,ip,Mul hs) =
  let es = map (mexpExp . flip justLookup m) hs
  in foldl' (*) 1 (map (\e->eval m (args,ip,e)) es)
eval m (args,ip,Add hs) =
  let es = map (mexpExp . flip justLookup m) hs
  in foldl' (+) 0 (map (\e->eval m (args,ip,e)) es)
eval m (args,ip,Fun s hs) =
  let es = map (mexpExp . flip justLookup m) hs
      f = justLookup s ?functionMap      
  in f (map (\e->eval m (args,ip,e)) es)
eval m (args,ip,Sum is h) =
  let idx1lst (idx,start,end) = [(idx,v)| v <- [start..end]]
      sumip = traverse idx1lst is                
      ip' = map (ip ++) sumip
      e = justLookup h m
  in (foldl' (+) 0 . map (\i -> eval m (args,i,mexpExp e))) ip'

seval :: ( Storable a, HasTrie a, Num a
         , ?expHash :: Exp a :->: Hash
         , ?functionMap :: FunctionMap a
         ) =>
         Args a -> IdxPoint -> MExp a -> a
seval a ip e = eval (mexpMap e) (a,ip,mexpExp e)

evalVar :: (Num a, VS.Storable a) => Args a -> IdxPoint -> Symbol -> a
evalVar args ip (Simple s) = (justLookup s . varSimple) args
evalVar args ip (Indexed s is) = let i's = map (flip justLookupL ip . view _1) is
                                     ival = justLookup s (varIndexed args)
                                 in valStore ival ! flatIndex ival i's

evalDelta ip i j = let i' = justLookupL i ip
                       j' = justLookupL j ip
                   in if i' == j' then 1 else 0 

