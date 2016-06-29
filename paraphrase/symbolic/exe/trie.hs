{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

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
import           Symbolic.Differential
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--


exp1 :: (?expHash :: Exp :->: Hash) => MExp
exp1 = square (x `add'` y)

exp2 :: (?expHash :: Exp :->: Hash) => MExp
exp2 = power 3 x -- power 10 (x `add'` y)

exp3 :: (?expHash :: Exp :->: Hash) => MExp
exp3 = ( (x_ "i") `mul'` (y_ "i")) `add'` ( (x_ "i") `mul` (x_ "i"))

exp4 :: (?expHash :: Exp :->: Hash) => MExp
exp4 = sum_ ["i"] exp3


expfib' :: (?expHash :: Exp :->: Hash) => (Int :->: MExp) -> Int -> MExp
expfib' _ 0 = x
expfib' _ 1 = y
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add e1 e2

expfib :: (?expHash :: Exp :->: Hash) => Int -> MExp 
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib

dexpfib' :: (?expHash :: Exp :->: Hash) => 
            (Int :->: MExp, (Symbol,Exp) :->: MExp)
         -> (Symbol,Int) -> MExp
dexpfib' (tfib,tdiff) (s,n) = let MExp e m _ = untrie tfib n in diff' m tdiff (s,e)

dexpfib :: (?expHash :: Exp :->: Hash) => (Symbol,Int) -> MExp
dexpfib (s,n) = let tfib = trie ffib
                    ffib = expfib' tfib
                    MExp _ m _ = untrie tfib n
                    tdiff = trie (diff' m tdiff)
                    f = dexpfib' (tfib,tdiff) 
                in f (s,n)

prettyPrintR = (prettyPrint . exp2RExp) >=> const endl

endl = putStrLn ""

main' :: IO ()
main' = do
    let ?expHash = trie hash
    -- digraph exp1
    -- digraph (expfib 100)
    -- digraph exp1
    digraph exp2

main :: IO ()
main = do
    let ?expHash = trie hash
    {- -- let MExp e m _ = exp1
    --    diff = fix (diff' m . trie)
    putStrLn . prettyPrint . exp2RExp $ sdiff (Simple "x") exp1
    let lexp1 = expfib 100
        lexp2 = dexpfib (Simple "y",100)
    -- prettyPrintR $ lexp1
    -- prettyPrintR $ lexp2    
    (printf "lexp2: %x\n" . untrie ?expHash . mexpExp) lexp2

    --let MExp e' m' _ = exp2
    --     ndiff = fix (diff' m' . trie)
    prettyPrintR $ sdiff (Simple "x") exp2

    --let MExp e3 m3 _ = exp3
    --    diff3 = fix (diff' m3 . trie) 

    
    let r = sdiff (Indexed "x" "j") exp3
    prettyPrintR r
    digraph r   -}


    
    
    printf "f = %s\n" ((prettyPrint . exp2RExp) exp4 :: String)
    let r' = sdiff (Indexed "x" "j") exp4
    printf "df/d(x_j) = %s\n" ((prettyPrint . exp2RExp) r' :: String)

    digraph r'

    -- print (mexpIdx r')
