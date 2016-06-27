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
import           Data.Monoid
import           Text.Printf
--
import           Predefined
import           Print
import           Type
--
import Debug.Trace

suffix' = (<> "'")

suffix_1 = (<> "_1")

suffix_2 = (<> "_2")
 
diff :: (?expHash :: Exp :->: Hash) => Symbol -> ExpMap -> ExpMap
diff s (ExpMap e m) =
  case e of
    Zero -> zero 
    One ->  zero
    Val n -> zero
    Var s' -> if s == s' then one else zero
    Fun1 f h1 -> let Just e1 = HM.lookup h1 m
                 in ExpMap (Fun1 (suffix' f) h1) m `mul` diff s e1
    Fun2 f h1 h2 -> let Just e1 = HM.lookup h1 m
                        Just e2 = HM.lookup h2 m
                    in (ExpMap (Fun2 (suffix_1 f) h1 h2) m `mul` diff s e1) `add`
                       (ExpMap (Fun2 (suffix_2 f) h1 h2) m `mul` diff s e2) 



exp1 :: (?expHash :: Exp :->: Hash) => ExpMap
exp1 = square (x `add` y)

exp2 :: (?expHash :: Exp :->: Hash) => ExpMap
exp2 = power 10 (x `add` y)


expfib' :: (?expHash :: Exp :->: Hash) => (Int :->: ExpMap) -> Int -> ExpMap
expfib' t 0 = val 0
expfib' t 1 = val 1
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add e1 e2

expfib :: (?expHash :: Exp :->: Hash) => Int -> ExpMap 
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib



digraph v = do
    let h = untrie ?expHash (expMapExp v)
        m = HM.insert h v (expMapMap v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (dotPrint m h) HS.empty
    putStrLn "}"

main' = do
    let ?expHash = trie hash

    -- digraph exp1
    -- digraph (expfib 100)
    -- digraph exp1
    digraph exp2
 
main = do
    let ?expHash = trie hash
    let -- (e,m) = exp2
        e = diff "x" exp2
        e2 = diff "x" (expfib 100)
    putStrLn . prettyPrint . exp2RExp $ e
    printf "%x\n" . untrie ?expHash . expMapExp $ e2

    -- putStrLn (prettyPrint (fst exp1))
    -- putStrLn (prettyPrint (fst x))
    -- printf "%x \n" (untrie ?expHash (Val 1))


