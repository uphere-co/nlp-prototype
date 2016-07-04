{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Monad ((>=>))
import           Control.Monad.Trans.State
import           Data.Foldable             (forM_)
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.List                 (lookup,foldl')
import           Data.Maybe                (fromJust)
import           Data.MemoTrie
import           Data.Monoid               ((<>))
import           Data.Vector.Storable       (Vector(..),Storable(..),(!))
import qualified Data.Vector.Storable as VS
import           Text.Printf
--
import           Symbolic.Differential
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Simplify
import           Symbolic.Type
--
import           Debug.Trace

-- data Args = Args { varMap :: HashMap Symbol Int }

type IdxPoint = [(Index,Int)]

data IdxVal a = IdxVal { indexRange :: [(Int,Int)]   -- range of indices (start,end)
                       , flatIndex :: [Int] -> Int
                       , valStore :: Vector a }

data Args a = Args { varSimple :: HashMap String a
                   , varIndexed :: HashMap String (IdxVal a) }

justLookupL :: (Eq k) => k -> [(k,v)] -> v
justLookupL k = fromJust . lookup k

eval :: (Num a, Storable a, HasTrie a, ?expHash :: Exp a :->: Hash) =>
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
eval m (args,ip,Sum is h) =
  let idx1lst (idx,start,end) = [(idx,v)| v <- [start..end]]
      sumip = traverse idx1lst is                
      ip' = map (ip ++) sumip
      e = justLookup h m
  in (foldl' (+) 0 . map (\i -> eval m (args,i,mexpExp e))) ip'

seval :: (Storable a, HasTrie a, Num a, ?expHash :: Exp a :->: Hash) =>
         Args a -> IdxPoint -> MExp a -> a
seval a ip e = eval (mexpMap e) (a,ip,mexpExp e)

evalVar :: (Num a, VS.Storable a) => Args a -> IdxPoint -> Symbol -> a
evalVar args ip (Simple s) = ({- EVal . -} justLookup s . varSimple) args
evalVar args ip (Indexed s is) = let i's = map (flip justLookupL ip) is
                                     ival = justLookup s (varIndexed args)
                                 in valStore ival ! flatIndex ival i's

evalDelta ip i j = let i' = justLookupL i ip
                       j' = justLookupL j ip
                   in if i' == j' then 1 else 0 


-- eval m t (args,ip,Fun1 f h1) =
--   let e1 = mexpExp (justLookup h1 m)
--   in case untrie t (args,ip,e1) of
--        EVal e1' -> if | f == "tanh" -> EVal (tanh e1')
--                       | otherwise   -> error (f ++ " is not supported yet")
--       _ -> error "invalid function application"
-- eval m t (args,ip,Fun2 o h1 h2) =
--   let e1 = mexpExp (justLookup h1 m)
--      e2 = mexpExp (justLookup h2 m)
--   in case (untrie t (args,ip,e1), untrie t (args,ip,e2)) of
--       (EVal e1',EVal e2') -> if | o == "+" -> EVal (e1' + e2')
--                                 | o == "*" -> EVal (e1' * e2')
--                                 | otherwise -> error "not supported"
--       _ -> error "strange" 
-- eval args (Sum _ _) = 



{- 
-}

{- 





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

exp2 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp2 = power 3 x -- power 10 (x `add'` y)

test2 :: IO ()
test2 = do
    let ?expHash = trie hash
    -- digraph exp1
    -- digraph (expfib 100)
    -- digraph exp1
    digraph (exp2 :: MExp Int)

test8 :: IO ()
test8 = do
  let ?expHash = trie hash
  let e1 = add' [x_ ["i"], zero,  y_ ["i"], x_ ["j"], zero] 
  printf "e1 = %s\n" ((prettyPrint . exp2RExp) (e1 ::  MExp Int) :: String)

  let e2 = mul' [x_ ["i"], one,  y_ ["j"], x_ ["i"], one] 
  printf "e2 = %s\n" ((prettyPrint . exp2RExp) (e2 ::  MExp Int) :: String)
  -- digraph e2

  let e3 = mul [x, x, mul [x, x , x] , x]
      de3 = (sdiff (Simple "x") e3 ::  MExp Int)
  printf "e3 = %s\n" ((prettyPrint . exp2RExp) (e3 ::  MExp Int) :: String)
  printf "d(e3)/dx = %s\n" ((prettyPrint . exp2RExp) de3  :: String)

  digraph de3


test9 :: IO ()
test9 = do
  let ?expHash = trie hash
  let e1 = mul [delta "i" "m", delta "j" "n"]
      e2 = mul [delta "i" "n", delta "j" "m"]
      e3 = add [e1,e2]
      e4 = mul [e3,x_ ["m","n"]]
  printf "e4 = %s\n"  ((prettyPrint . exp2RExp) (e4 ::  MExp Int) :: String)

  digraph e4


test10 :: IO ()
test10 = do
  let ?expHash = trie hash
  let e = mul [x, y] :: MExp Int
      args = Args (HM.fromList [("x",2),("y",3)]) (HM.empty)
  printf "e = %s\n"  ((prettyPrint . exp2RExp) e :: String)
  
  printf "val(e) = %d\n" (eval (mexpMap e) (args,[],mexpExp e))

test11 :: IO ()
test11 = do
  let ?expHash = trie hash
  let e1 = mul [delta "i" "m", delta "j" "n"] :: MExp Int
      e2 = mul [val (-1), delta "i" "n", delta "j" "m"]
      e3 = add [e1,e2]
      e4 = mul [e3,x_ ["m","n"]]
      e5 = sum_ [("m",1,2),("n",1,2)] e4
  printf "e5 = %s\n"  ((prettyPrint . exp2RExp) e5 :: String)
  let vals = IdxVal [(1,2),(1,2)] (\[i,j] -> (i-1)*2+(j-1)) (VS.fromList [1,2,3,4])
      args = Args HM.empty (HM.fromList [("x",vals)])
  forM_ [(1,1),(1,2),(2,1),(2,2)] $ \(i,j) -> do
    let idx = [("i",i),("j",j)] 
    printf "val(e5(i=%d,j=%d) = %d\n" i j (seval args idx e5)


test12 :: IO ()
test12 = do
  let ?expHash = trie hash
  let e1 :: MExp Double
      e1 = add' [x,y]
      fe1 = fun "f" [e1,x]
      dfe1 = sdiff (Simple "x") fe1
  printf "fe1 = %s\n"  ((prettyPrint . exp2RExp) fe1 :: String)
  printf "d(fe1)/dx = %s\n" ((prettyPrint . exp2RExp) dfe1 :: String)
  digraph dfe1
  
main = test12 
    
