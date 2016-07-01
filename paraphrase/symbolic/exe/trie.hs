{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
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
import           Data.List                 (lookup)
import           Data.Maybe                (fromJust)
import           Data.MemoTrie
import           Data.Monoid               ((<>))
import           Text.Printf
--
import           Symbolic.Differential
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Simplify
import           Symbolic.Type
--

-- data Args = Args { varMap :: HashMap Symbol Int }
type Args = [(Symbol,Double)]
type IdxPoint = [(Index,Int)]

justLookupL k = fromJust . lookup k

eval :: (?expHash :: Exp :->: Hash) =>
        HashMap Hash MExp -> ((Args,Exp) :->: Double) -> Args -> Exp -> Double
eval _ _ _ Zero = 0
eval _ _ _ One  = 1
-- eval _ (mexpExp -> Delta i j) = 
eval m t args (Var s) = justLookupL s args
eval m t args (Fun1 f h1) =
  let e1 = mexpExp (justLookup h1 m)
  in if | f == "tanh" -> tanh (untrie t (args,e1))
        | otherwise   -> error (f ++ " is not supported yet")
eval m t args (Fun2 o h1 h2) =
  let e1 = mexpExp (justLookup h1 m)
      e2 = mexpExp (justLookup h2 m)
  in if | o == "+" -> untrie t (args,e1) + untrie t (args,e2)
        | o == "*" -> untrie t (args,e1) * untrie t (args,e2)
        | otherwise -> error "not supported"
-- eval args (Sum _ _) = 



exp1 :: (?expHash :: Exp :->: Hash) => MExp
exp1 = square (x `add'` y)

exp2 :: (?expHash :: Exp :->: Hash) => MExp
exp2 = power 3 x -- power 10 (x `add'` y)

exp3 :: (?expHash :: Exp :->: Hash) => MExp
exp3 = (x_ ["i"] `mul'` y_ ["i"]) `add'` (x_ ["i"] `mul` x_ ["i"])

exp4 :: (?expHash :: Exp :->: Hash) => MExp
exp4 = sum_ ["i"] exp3

exp5 :: (?expHash :: Exp :->: Hash) => MExp
exp5 = sum_ ["i","j"] ((x_ ["i"] `mul` y_ ["i","j"]) `mul` x_ ["j"])


expfib' :: (?expHash :: Exp :->: Hash) => (Int :->: MExp) -> Int -> MExp
expfib' _ 0 = x -- x_ ["i"]
expfib' _ 1 = y -- y_ ["i"]
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

eval_fib :: (?expHash :: Exp :->: Hash) => Args -> Int -> Double
eval_fib a n = let tfib = trie ffib
                   ffib = expfib' tfib
                   e = mexpExp (ffib n)
                   m = mexpMap (ffib n)
                   feval = uncurry (eval m teval)
                   teval = trie feval 
               in untrie teval (a,e)


prettyPrintR = (prettyPrint . exp2RExp) >=> const endl

endl = putStrLn ""


mkDepGraph :: (?expHash :: Exp :->: Hash) => MExp -> [(Hash,Hash)]
mkDepGraph e = let e1 = mexpExp e
                   h1 = untrie ?expHash e1
                   m1 = mexpMap e
                   hs = daughters e1
                   lst = map (h1,) hs
                   lsts = map (mkDepGraph . flip justLookup m1) hs 
               in concat (lst:lsts)

-- revDep :: HashMap Hash Hash -> Hash -> [Hash]

test_digraph :: IO ()
test_digraph = do
    let ?expHash = trie hash
    -- digraph exp1
    -- digraph (expfib 100)
    -- digraph exp1
    digraph exp2

test123 :: IO ()
test123 = do
    let ?expHash = trie hash
    -- let MExp e m _ = exp1
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
    
test3 = do
  let ?expHash = trie hash
  let r = sdiff (Indexed "x" ["j"]) exp3
  prettyPrintR r
  digraph r  


test4 = do
  let ?expHash = trie hash
  
  printf "f = %s\n" ((prettyPrint . exp2RExp) exp4 :: String)
  let r' = sdiff (Indexed "x" ["j"]) exp4
  printf "df/d(x_j) = %s\n" ((prettyPrint . exp2RExp) r' :: String)

  digraph r'

  -- print (mexpIdx r')

test5 = do
  let ?expHash = trie hash  
  printf "f = %s\n" ((prettyPrint . exp2RExp) exp5 :: String)
  let r = sdiff (Indexed "y" ["m","n"]) exp5
  printf "df/d(y_mn) = %s\n" ((prettyPrint . exp2RExp) r :: String)
  digraph r

  mapM_ (\(h1,h2) -> printf "x%x -> x%x\n" h1 h2) $ mkDepGraph r

test6 :: IO ()
test6 = do
  let ?expHash = trie hash    
  -- let lexp1 = expfib 10
  let n = 3
      lexp1 = expfib n
      lexp2 = dexpfib (Indexed "x" ["j"],n)
  prettyPrintR $ lexp1
  prettyPrintR $ lexp2    
  (printf "lexp2: %x\n" . untrie ?expHash . mexpExp) lexp2

test7 :: IO ()
test7 = do
  let ?expHash = trie hash
  let n = 100
      -- lexp1 = expfib n
      -- lexp2 = dexpfib (Indexed "x" ["j"],n)
  
  -- prettyPrintR lexp1
  let args = [(Simple "x",1),(Simple "y",1)]
     
  printf "f(1,1) = %e\n" $ eval_fib args n -- eval args lexp1


main = do
  test7

    
