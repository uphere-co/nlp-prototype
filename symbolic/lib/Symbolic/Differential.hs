{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Differential where

import           Data.Function             (fix)
import           Data.HashMap.Strict       (HashMap)
import           Data.MemoTrie
--
import           Symbolic.Predefined
import           Symbolic.Simplify
import           Symbolic.Type
--

diff'
  :: forall a. (HasTrie a, Num a, ?expHash :: Exp a :->: Hash)
  => HashMap Hash (MExp a)
  -> ((Symbol,Exp a) :->: MExp a)
  -> (Symbol,Exp a) -> MExp a
diff' m t (s,e) =
  case e of
    Zero         -> zero 
    One          -> zero
    Delta _ _    -> zero
    CDelta _ _ _ -> zero    
    Val _        -> zero
    Var s'       -> dvar s s'
    Add hs       -> let es = map (flip justLookup m) hs
                    in add' (map (\e' -> untrie t (s,mexpExp e')) es)
    Mul hs       -> let es = map (flip justLookup m) hs
                    in add' (diffmul es)
    Fun sym hs   -> let ies = zip [1..] $ map (flip justLookup m) hs
                    in add' (difff sym ies)
    Sum is h1    -> let MExp e1 _ _ = justLookup h1 m
                    in sum_ is (untrie t (s,e1))
    Concat i hs  -> undefined
    {- let es = map (flip justLookup m) hs
                        i' = index0base idx (justLookupL (view _1 idx) ip)
                        dis = map (HS.toList . mexpIdx) es 
                        di = splitIndexDisjoint dis i'
                        select (x:xs) (L i)
                          = let i' = zipWith (\(k,_,_) v -> (k,v)) (HS.toList (mexpIdx x)) i
                            in eval m (args,i',mexpExp x)
                        select (x:xs) (R d) = select xs d
                    in error "diff': Concat not implemented" -}
 where
  diffmul :: [MExp a] -> [MExp a] 
  diffmul [] = []
  diffmul (x1:xs) = let x' = untrie t (s,mexpExp x1)
                        xs'all = diffmul xs
                    in (mul' (x':xs) : map (\y1 -> mul' [x1,y1]) xs'all)    

  difff :: String -> [(Int,MExp a)] -> [MExp a]
  difff sym args = map (difff' sym (map snd args)) args

  difff' :: String -> [MExp a] -> (Int,MExp a) -> MExp a
  difff' sym args (i,e1) = let e' = untrie t (s,mexpExp e1)
                           in mul' [fun (suffix_n i sym) args , e'] 
                      

-- | differentiation of variables
dvar :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Symbol -> Symbol -> MExp a
dvar (Simple s)    (Simple s')   = if s == s' then one else zero
dvar (Simple _)    _             = zero
dvar _             (Simple _ )   = zero
dvar (Indexed x1 j) (Indexed y1 k)
  | x1 == y1 && length j == length k = let djk = zipWith delta j k
                                       in mul' djk
  | otherwise = zero

-- | simple differentiation without complex memoization
sdiff :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Symbol -> MExp a -> MExp a
sdiff s (MExp e m _) = let diff = fix (diff' m . trie) in diff (s,e)

