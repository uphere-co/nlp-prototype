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
import           Data.Monoid
import           Text.Printf
--
import           Predefined
import           Print
import           Type
--

suffix' :: Symbol -> Symbol
suffix' (Simple s) = Simple (s <> "'")

suffix_1 :: Symbol -> Symbol
suffix_1 (Simple s) = Simple (s <> "_1")

suffix_2 :: Symbol -> Symbol
suffix_2 (Simple s) = Simple (s  <> "_2")
                    
diff'
  :: (?expHash :: Exp :->: Hash)
  => HashMap Hash ExpMap
  -> ((Symbol,Exp) :->: ExpMap)
  -> (Symbol,Exp) -> ExpMap
diff' m t (s,e) =
  case e of
    Zero         -> zero 
    One          -> zero
    Val _        -> zero
    Var s'       -> dvar s s' 
    Fun1 f h1    -> let ExpMap e1 _ _ = justLookup h1 m
                    in ExpMap (Fun1 (suffix' f) h1) m HS.empty `mul'` untrie t (s,e1)
    Fun2 f h1 h2 -> let ExpMap e1 _ _ = justLookup h1 m
                        ExpMap e2 _ _ = justLookup h2 m
                    in (simplify2 m f Pos1 h1 h2 `mul'` untrie t (s,e1)) `add'`
                         (simplify2 m f Pos2 h1 h2 `mul'` untrie t (s,e2)) 

dvar (Simple s)    (Simple s')   = if s == s' then one else zero
dvar (Simple s)    _             = zero
dvar _             (Simple s')   = zero
dvar (Indexed x j) (Indexed y k) = if x == y then delta j k else zero

data Pos = Pos1 | Pos2 


simplify2 :: HashMap Hash ExpMap -> Symbol -> Pos -> Hash -> Hash -> ExpMap
simplify2 m f pos h1 h2
  | showSym f == "+" = one
  | showSym f == "*" = case pos of
                         Pos1 -> justLookup h2 m
                         Pos2 -> justLookup h1 m
  | otherwise        = case pos of
                         Pos1 -> ExpMap (Fun2 (suffix_1 f) h1 h2) m HS.empty
                         Pos2 -> ExpMap (Fun2 (suffix_2 f) h1 h2) m HS.empty

add' :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap -> ExpMap
add' e1                   (expMapExp -> Zero)  = e1
add' (expMapExp -> Zero)  e2                   = e2
add' (expMapExp -> One)   (expMapExp -> One)   = val 2
add' (expMapExp -> Val m) (expMapExp -> One)   = val (m+1)
add' (expMapExp -> One)   (expMapExp -> Val m) = val (m+1)
add' (expMapExp -> Val m) (expMapExp -> Val n) = val (m+n)
add' e1                   e2                   = e1 `add` e2

mul' :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap -> ExpMap
mul' _                   (expMapExp -> Zero) = zero
mul' (expMapExp -> Zero) _                   = zero
mul' e1                  (expMapExp -> One)  = e1
mul' (expMapExp -> One)  e2                  = e2
mul' e1                  e2                  = e1 `mul` e2 

exp1 :: (?expHash :: Exp :->: Hash) => ExpMap
exp1 = square (x `add'` y)

exp2 :: (?expHash :: Exp :->: Hash) => ExpMap
exp2 = power 3 x -- power 10 (x `add'` y)

exp3 :: (?expHash :: Exp :->: Hash) => ExpMap
exp3 = ( (x_ "i") `mul'` (y_ "i")) `add'` ( (x_ "i") `mul` (x_ "i"))

expfib' :: (?expHash :: Exp :->: Hash) => (Int :->: ExpMap) -> Int -> ExpMap
expfib' _ 0 = x
expfib' _ 1 = y
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
dexpfib' (tfib,tdiff) (s,n) = let ExpMap e m _ = untrie tfib n in diff' m tdiff (s,e)

dexpfib :: (?expHash :: Exp :->: Hash) => (Symbol,Int) -> ExpMap
dexpfib (s,n) = let tfib = trie ffib
                    ffib = expfib' tfib
                    ExpMap _ m _ = untrie tfib n
                    tdiff = trie (diff' m tdiff)
                    f = dexpfib' (tfib,tdiff) 
                in f (s,n)

prettyPrintR = (prettyPrint . exp2RExp) >=> const endl

endl = putStrLn ""

digraph :: (?expHash :: Exp :->: Hash) => ExpMap -> IO ()
digraph v = do
    let h = untrie ?expHash (expMapExp v)
        m = HM.insert h v (expMapMap v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (dotPrint m h) HS.empty
    putStrLn "}"

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
    let ExpMap e m _ = exp1
        diff = fix (diff' m . trie)
    putStrLn . prettyPrint . exp2RExp $ diff (Simple "x",e)
    let lexp1 = expfib 6
        lexp2 = dexpfib (Simple "y",6)    
    prettyPrintR $ lexp1
    prettyPrintR $ lexp2    
    (printf "x : %x\n" . untrie ?expHash . expMapExp) lexp2

    let ExpMap e' m' _ = exp2
        ndiff = fix (diff' m' . trie)
    prettyPrintR $ ndiff (Simple "x",e')

    let ExpMap e3 m3 _ = exp3
        diff3 = fix (diff' m3 . trie)
        r = diff3 (Indexed "x" "j",e3)
    prettyPrintR r
    digraph r
