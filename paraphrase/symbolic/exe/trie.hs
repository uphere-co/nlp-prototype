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
type Args a = [(Symbol,a)]
type IdxPoint = [(Index,Int)]

justLookupL :: (Eq k) => k -> [(k,v)] -> v
justLookupL k = fromJust . lookup k

{- 
eval :: (Num a, Floating a, HasTrie a, ?expHash :: Exp a :->: Hash) =>
        HashMap Hash (MExp a) -> ((Args a,IdxPoint,Exp a) :->: EExp a) -> (Args a,IdxPoint,Exp a) -> EExp a
eval _ _ (_,_,Zero) = EVal 0
eval _ _ (_,_,One)  = EVal 1
eval _ _ (_,_,Val n) = EVal n
eval _ _ (_,ip,Delta i j) = evalDelta ip i j 
eval m t (args,ip,Var s) = EVal (justLookupL s args)
eval m t (args,ip,Fun1 f h1) =
  let e1 = mexpExp (justLookup h1 m)
  in case untrie t (args,ip,e1) of
       EVal e1' -> if | f == "tanh" -> EVal (tanh e1')
                      | otherwise   -> error (f ++ " is not supported yet")
       _ -> error "invalid function application"
eval m t (args,ip,Fun2 o h1 h2) =
  let e1 = mexpExp (justLookup h1 m)
      e2 = mexpExp (justLookup h2 m)
  in case (untrie t (args,ip,e1), untrie t (args,ip,e2)) of
       (EVal e1',EVal e2') -> if | o == "+" -> EVal (e1' + e2')
                                 | o == "*" -> EVal (e1' * e2')
                                 | otherwise -> error "not supported"
       _ -> error "strange" 
-- eval args (Sum _ _) = 

evalDelta ip i j = let mi = lookup i ip
                       mj = lookup j ip
                   in case (mi,mj) of
                        (Just i',Just j') -> if i'==j' then EVal 1 else EVal 0
                        (Just i',Nothing) -> EDelta (TDelta1 j i')
                        (Nothing,Just j') -> EDelta (TDelta1 i j')
                        (Nothing,Nothing) -> EDelta (TDelta2 i j)


exp1 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp1 = square (x `add'` y)

exp1' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp1' = tanh_ exp1

exp2 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp2 = power 3 x -- power 10 (x `add'` y)

exp3 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp3 = (x_ ["i"] `mul'` y_ ["i"]) `add'` (x_ ["i"] `mul` x_ ["i"])

exp4 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp4 = sum_ ["i"] exp3

exp5 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp5 = sum_ ["i","j"] ((x_ ["i"] `mul` y_ ["i","j"]) `mul` x_ ["j"])


expfib' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (Int :->: MExp a) -> Int -> MExp a
expfib' _ 0 = x -- x_ ["i"]
expfib' _ 1 = y -- y_ ["i"]
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add e1 e2

expfib :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Int -> MExp a
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib

dexpfib' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => 
            (Int :->: MExp a, (Symbol,Exp a) :->: MExp a)
         -> (Symbol,Int) -> MExp a
dexpfib' (tfib,tdiff) (s,n) = let MExp e m _ = untrie tfib n in diff' m tdiff (s,e)

dexpfib :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (Symbol,Int) -> MExp a
dexpfib (s,n) = let tfib = trie ffib
                    ffib = expfib' tfib
                    MExp _ m _ = untrie tfib n
                    tdiff = trie (diff' m tdiff)
                    f = dexpfib' (tfib,tdiff) 
                in f (s,n)

eval_fib :: (HasTrie a, Num a, Floating a, ?expHash :: Exp a :->: Hash) => Args a -> IdxPoint -> Int -> EExp a
eval_fib a ip n = let tfib = trie ffib
                      ffib = expfib' tfib
                      e = mexpExp (ffib n)
                      m = mexpMap (ffib n)
                      feval = eval m teval
                      teval = trie feval 
                  in untrie teval (a,ip,e)


seval :: (HasTrie a, Num a, Floating a, ?expHash :: Exp a :->: Hash) =>
         Args a -> IdxPoint -> MExp a -> EExp a
seval a ip e = let f = fix (eval (mexpMap e) . trie)
               in f (a,ip,mexpExp e)



prettyPrintR = (prettyPrint . exp2RExp) >=> const endl

endl = putStrLn ""


mkDepGraph :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> [(Hash,Hash)]
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
    digraph (exp2 :: MExp Int)

test123 :: IO ()
test123 = do
    let ?expHash = trie hash
    -- let MExp e m _ = exp1
    --    diff = fix (diff' m . trie)
    putStrLn . prettyPrint . exp2RExp $ sdiff (Simple "x") exp1
    let lexp1 = expfib 100 :: MExp Int
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
  let r = sdiff (Indexed "x" ["j"]) (exp3 :: MExp Int)
  prettyPrintR r
  digraph r  


test4 = do
  let ?expHash = trie hash
  
  printf "f = %s\n" ((prettyPrint . exp2RExp) (exp4 :: MExp Int) :: String)
  let r' = sdiff (Indexed "x" ["j"]) exp4
  printf "df/d(x_j) = %s\n" ((prettyPrint . exp2RExp) r' :: String)

  digraph r'

  -- print (mexpIdx r')

test5 = do
  let ?expHash = trie hash  
  printf "f = %s\n" ((prettyPrint . exp2RExp) (exp5 ::  MExp Int) :: String)
  let r = sdiff (Indexed "y" ["m","n"]) exp5
  printf "df/d(y_mn) = %s\n" ((prettyPrint . exp2RExp) r :: String)
  digraph r

  mapM_ (\(h1,h2) -> printf "x%x -> x%x\n" h1 h2) $ mkDepGraph r

test6 :: IO ()
test6 = do
  let ?expHash = trie hash    
  -- let lexp1 = expfib 10
  let n = 3
      lexp1 = expfib n :: MExp Int
      lexp2 = dexpfib (Indexed "x" ["j"],n)
  prettyPrintR $ lexp1
  prettyPrintR $ lexp2    
  (printf "lexp2: %x\n" . untrie ?expHash . mexpExp) lexp2

test7 :: IO ()
test7 = do
  let ?expHash = trie hash :: Exp Double :->: Hash
  let n = 100
      -- lexp1 = expfib n
      -- lexp2 = dexpfib (Indexed "x" ["j"],n)
  
  -- prettyPrintR lexp1
  let args = [(Simple "x",1),(Simple "y",1 :: Double)]
  -- let args' = [(Indexed "x" 1,1),(Indexed "y" 1,1 :: Double)]  
     
  prettyPrintE $ seval args [] exp3 -- eval_fib args n -- eval args lexp1



-}

test8 :: IO ()
test8 = do
  let ?expHash = trie hash
  let e1 = add' [x_ ["i"], zero,  y_ ["i"], x_ ["j"], zero] 
  printf "e1 = %s\n" ((prettyPrint . exp2RExp) (e1 ::  MExp Int) :: String)

  let e2 = mul' [x_ ["i"], one,  y_ ["j"], x_ ["i"], one] 
  printf "e2 = %s\n" ((prettyPrint . exp2RExp) (e2 ::  MExp Int) :: String)

  digraph e2


main = test8
    
