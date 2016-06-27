{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Arrow
import           Control.Lens              (over, _1)
import           Control.Monad.Trans.State
import           Data.Bits                 (xor)
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Text.Printf
--
import           Predefined
import           Print
import           Type
--
import Debug.Trace

  


exp1 :: (?expHash :: Exp :->: Int) => ExpMap
exp1 = square (x `add` y)

exp2 :: (?expHash :: Exp :->: Int) => ExpMap
exp2 = power 10 (x `add` y)


expfib' :: (?expHash :: Exp :->: Int) => (Int :->: ExpMap) -> Int -> ExpMap
expfib' t 0 = val 0 -- let e = Val 0; h = hash e in (HM.singleton h e, Val 0)
expfib' t 1 = val 1 -- let e = Val 1; h = hash e in (HM.singleton h e, Val 1)
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add e1 e2

expfib :: (?expHash :: Exp :->: Int) => Int -> ExpMap 
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib



main = do
    let ?expHash = trie hash
    let (e,m) = exp2
        (ex,mx) = x
    putStrLn . prettyPrint . exp2RExp m $ e
    putStrLn . prettyPrint . exp2RExp mx $ ex
    -- putStrLn (prettyPrint (fst exp1))
    -- putStrLn (prettyPrint (fst x))
    -- printf "%x \n" (untrie ?expHash (Val 1))

digraph v = do
    let -- v = expfib 10
        h = untrie ?expHash (fst v)
    let m = HM.insert h (fst v) (snd v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (dotPrint m h) HS.empty
    putStrLn "}"

main' = do
    let ?expHash = trie hash
    
    -- digraph (expfib 10)
    -- digraph exp1
    digraph exp2
