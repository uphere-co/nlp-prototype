{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Differential where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.State
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Data.Monoid               ((<>))
import           Text.Printf
--
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Simplify
import           Symbolic.Type
--
import           Debug.Trace

                   
diff'
  :: forall a. (HasTrie a, Num a, ?expHash :: Exp a :->: Hash)
  => HashMap Hash (MExp a)
  -> ((Symbol,Exp a) :->: MExp a)
  -> (Symbol,Exp a) -> MExp a
diff' m t (s,e) =
  case e of
    Zero         -> zero 
    One          -> zero
    Val _        -> zero
    Var s'       -> dvar s s'
    Add hs       -> let es = map (flip justLookup m) hs
                    in add' (map (\e -> untrie t (s,mexpExp e)) es)
    Mul hs       -> let es = map (flip justLookup m) hs
                    in add' (diffmul es)
    Fun sym hs   -> let ies = zip [1..] $ map (flip justLookup m) hs
                    in add' (difff sym ies)
    Sum is h1    -> let MExp e1 _ _ = justLookup h1 m
                    in sum_ is (untrie t (s,e1))
 where
  diffmul :: [MExp a] -> [MExp a] 
  diffmul [] = []
  diffmul (x:xs) = let x' = untrie t (s,mexpExp x)
                       xs'all = diffmul xs
                   in (mul' (x':xs) : map (\y -> mul' [x,y]) xs'all)    

  difff :: String -> [(Int,MExp a)] -> [MExp a]
  difff sym args = map (difff' sym (map snd args)) args

  difff' :: String -> [MExp a] -> (Int,MExp a) -> MExp a
  difff' sym args (i,e) = let e' = untrie t (s,mexpExp e)
                          in mul' [fun (suffix_n i sym) args , e'] 
                      
{- 
                          difff :: [MExp a] -> [MExp a] 
                        difff [] = []
                        difff (x:xs) = let x' = untrie t (s,mexpExp x)
                                           xs'all = difff xs
                                       in (mul' (x':xs) : map (\y -> mul' [x,y]) xs'all)    
-}
--                      add' (diffmul es)
                    
--    Add hs       -> let es = map (flip justLookup m) hs
--                    in add' (map (\e -> untrie t (s,mexpExp e)) es)
                       
{-     Fun1 f h1    -> let MExp e1 _ _ = justLookup h1 m
                    in MExp (Fun1 (suffix' f) h1) m HS.empty `mul'` untrie t (s,e1)
    Fun2 f h1 h2 -> let MExp e1 _ _ = justLookup h1 m
                        MExp e2 _ _ = justLookup h2 m
                    in (simplify2 m f Pos1 h1 h2 `mul'` untrie t (s,e1)) `add'`
                         (simplify2 m f Pos2 h1 h2 `mul'` untrie t (s,e2))  -}



dvar :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Symbol -> Symbol -> MExp a
dvar (Simple s)    (Simple s')   = if s == s' then one else zero
dvar (Simple s)    _             = zero
dvar _             (Simple s')   = zero
dvar (Indexed x j) (Indexed y k)
  | x == y && length j == length k = let djk = zipWith delta j k
                                     in mul' djk
  | otherwise = zero


sdiff :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Symbol -> MExp a -> MExp a
sdiff s (MExp e m _) = let diff = fix (diff' m . trie) in diff (s,e)

