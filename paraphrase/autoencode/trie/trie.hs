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
import           Data.Maybe                (fromJust)
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
 
diff'
  :: (?expHash :: Exp :->: Hash)
  => HashMap Hash ExpMap
  -> ((Symbol,Exp) :->: ExpMap)
  -> (Symbol,Exp) -> ExpMap
diff' m t (s,e) =
  case e of
    Zero -> zero 
    One ->  zero
    Val n -> zero
    Var s' -> if s == s' then one else zero
    Fun1 f h1 -> let Just (ExpMap e1 _) = HM.lookup h1 m
                 in ExpMap (Fun1 (suffix' f) h1) m `mul'` untrie t (s,e1)
    Fun2 f h1 h2 -> let Just (ExpMap e1 _) = HM.lookup h1 m
                        Just (ExpMap e2 _) = HM.lookup h2 m
                    in (simplify2 m f Pos1 h1 h2 `mul'` untrie t (s,e1)) `add'`
                         (simplify2 m f Pos2 h1 h2 `mul'` untrie t (s,e2)) 

data Pos = Pos1 | Pos2 

justLookup h m = fromJust (HM.lookup h m)  -- this is very unsafe, but we do not have good solution yet.

simplify2 m f pos h1 h2
  | f == "+" = one
  | f == "*" = case pos of
                 Pos1 -> justLookup h2 m
                 Pos2 -> justLookup h1 m
  | otherwise = case pos of
                  Pos1 -> ExpMap (Fun2 (suffix_1 f) h1 h2) m
                  Pos2 -> ExpMap (Fun2 (suffix_2 f) h1 h2) m

add' e1                 (ExpMap Zero _)    = e1
add' (ExpMap Zero _)    e2                 = e2
add' (ExpMap One _)     (ExpMap One _)     = val 2
add' (ExpMap (Val m) _) (ExpMap One _)     = val (m+1)
add' (ExpMap One _)     (ExpMap (Val m) _) = val (m+1)
add' (ExpMap (Val m) _) (ExpMap (Val n) _) = val (m+n)
add' e1               e2               = e1 `add` e2

mul' e1              (ExpMap Zero _) = zero
mul' (ExpMap Zero _) e2              = zero
mul' e1              (ExpMap One  _) = e1
mul' (ExpMap One  _) e2              = e2
mul' e1              e2              = e1 `mul` e2 

exp1 :: (?expHash :: Exp :->: Hash) => ExpMap
exp1 = square (x `add` y)

exp2 :: (?expHash :: Exp :->: Hash) => ExpMap
exp2 = power 10 (x `add` y)


expfib' :: (?expHash :: Exp :->: Hash) => (Int :->: ExpMap) -> Int -> ExpMap
expfib' t 0 = x
expfib' t 1 = y
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add e1 e2

expfib :: (?expHash :: Exp :->: Hash) => Int -> ExpMap 
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib


dexpfib' :: (?expHash :: Exp :->: Hash) => 
            (Int :->: ExpMap, (Symbol,Exp) :->: ExpMap)
         -> (Symbol,Int) -> ExpMap
dexpfib' (tfib,tdiff) (s,n) = let ExpMap e m = untrie tfib n in diff' m tdiff (s,e)

dexpfib :: (?expHash :: Exp :->: Hash) => (Symbol,Int) -> ExpMap
dexpfib (s,n) = let tfib = trie expfib
                    expfib = expfib' tfib
                    ExpMap e m = untrie tfib n
                    tdiff = trie (diff' m tdiff)
                    f = dexpfib' (tfib,tdiff) 
                in f (s,n)

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
    let ExpMap e m = exp1
        diff = fix (diff' m . trie)
    putStrLn . prettyPrint . exp2RExp $ diff ("x",e)
    let lexp1 = expfib 6
        lexp2 = dexpfib ("y",6)    
    putStrLn . prettyPrint . exp2RExp $ lexp1
    putStrLn . prettyPrint . exp2RExp $ lexp2    
    (printf "x : %x\n" . untrie ?expHash . expMapExp) lexp2


{-
main = do
    let ?expHash = trie hash

    printf "x : %x\n" $ (hash (expMapExp x)) -- . untrie ?expHash . expMapExp $ e2
    printf "one: %x\n" $ (hash (expMapExp one))
    let -- (e,m) = exp2
        e = diff "x" (y `add` x) -- exp1 -- exp2
        --e2 = diff "x" (expfib 10)
        e' = expfib 2
        e2 = dexpfib 2
    digraph e'
    digraph e2
        
    putStrLn . prettyPrint . exp2RExp $ e'
    putStrLn . prettyPrint . exp2RExp $ e2
    -- putStrLn . prettyPrint . exp2RExp $ e -- iff "x" x
    
    -- digraph e
    let eee = expfib 100
    printf "%x\n" . untrie ?expHash . expMapExp $ eee

    -- putStrLn (prettyPrint (fst exp1))
    -- putStrLn (prettyPrint (fst x))
    -- printf "%x \n" (untrie ?expHash (Val 1))
-}

