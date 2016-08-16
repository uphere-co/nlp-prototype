{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test where



import           Data.Foldable             (forM_)

import           Data.Hashable

import qualified Data.HashMap.Strict as HM

import           Data.MemoTrie
import qualified Data.Vector.Storable as VS
import           Text.Printf
--
import           Symbolic.CodeGen.C
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Simplify
import           Symbolic.Type
import           Symbolic.Util
--

idxi, idxj, idxI, idxk, idxm, idxn :: Index
idxi = ("i",1,2)
idxj = ("j",1,2)

idxI = ("I",1,4)

idxk = ("k",1,2)
idxm = ("m",1,2)
idxn = ("n",1,2)


test2 :: IO ()
test2 = do
    let ?expHash = trie hash
    let exp2 :: MExp Int
        exp2 = power 3 varx -- power 10 (x `add'` y)
    
    digraph (exp2 :: MExp Int)

test8 :: IO ()
test8 = do
  let ?expHash = trie hash
  let e1 = add' [x_ [idxi], zero,  y_ [idxi], x_ [idxj], zero] 
  printf "e1 = %s\n" ((prettyPrint . exp2RExp) (e1 ::  MExp Int) :: String)

  let e2 = mul' [x_ [idxi], one,  y_ [idxj], x_ [idxi], one] 
  printf "e2 = %s\n" ((prettyPrint . exp2RExp) (e2 ::  MExp Int) :: String)
  -- digraph e2

  let e3 = mul [varx, varx, mul [varx, varx , varx] , varx]
      de3 = (sdiff (V (mkSym "x") []) e3 ::  MExp Int)
  printf "e3 = %s\n" ((prettyPrint . exp2RExp) (e3 ::  MExp Int) :: String)
  printf "d(e3)/dx = %s\n" ((prettyPrint . exp2RExp) de3  :: String)
  digraph de3

test9 :: IO ()
test9 = do
  let ?expHash = trie hash
  let e1 = mul [delta idxi idxm, delta idxj idxn]
      e2 = mul [delta idxi idxn, delta idxj idxm]
      e3 = add [e1,e2]
      e4 = mul [e3,x_ [idxm,idxn]]
  printf "e4 = %s\n"  ((prettyPrint . exp2RExp) (e4 ::  MExp Int) :: String)

  digraph e4


test10 :: IO ()
test10 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let e = mul [varx, vary] :: MExp Int
      args = Args (HM.fromList [(mkSym "x",VS.fromList [2])
                               ,(mkSym "y",VS.fromList [3])])
  printf "e = %s\n"  ((prettyPrint . exp2RExp) e :: String)
  
  printf "val(e) = %d\n" (eval (mexpMap e) (args,[],mexpExp e))

test11 :: IO ()
test11 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let e1 = mul [delta idxi idxm,  delta idxj idxn] :: MExp Int
      e2 = mul [val (-1), delta idxi idxn, delta idxj idxm]
      e3 = add [e1,e2]
      e4 = mul [e3,x_ [idxm,idxn]]
      e5 = sum_ [idxm,idxn] e4
  printf "e5 = %s\n"  ((prettyPrint . exp2RExp) e5 :: String)
  let vals = VS.fromList [1,2,3,4]
      args = Args (HM.fromList [(mkSym "x",vals)])
  forM_ [(1,1),(1,2),(2,1),(2,2)] $ \(i,j) -> do
    let idx = [("i",i),("j",j)] 
    printf "val(e5(i=%d,j=%d) = %d\n" i j (seval args idx e5)


test12 :: IO ()
test12 = do
  let ?expHash = trie hash
      ?functionMap = HM.fromList [ ("f", \[x,y] -> x*x + y*y)
                                 , ("f_1", \[x,_y] -> 2*x)
                                 , ("f_2", \[_x,y] -> 2*y) ]
  let e1 :: MExp Int
      e1 = add' [mul' [val 2,varx],vary]
      fe1 = fun "f" [e1,varx]
      dfe1 = sdiff (V (mkSym "x") []) fe1
  printf "fe1 = %s\n"  ((prettyPrint . exp2RExp) fe1 :: String)
  printf "d(fe1)/dy = %s\n" ((prettyPrint . exp2RExp) dfe1 :: String)
  --  digraph dfe1
  let args = Args (HM.fromList [(mkSym "x",VS.fromList [2])
                               ,(mkSym "y",VS.fromList [3])])
  
  printf "dfe1/dx(2,3)) = %d\n" (seval args [] dfe1)


test13 :: IO ()
test13 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty

  let exp0 :: MExp Double
      exp0 = y_ [idxm,idxn]
      exp1 = delta idxm idxn      
      exp2 = sum_ [idxm] (add' [exp0,exp1])
      exp3 = mul' [ z_ [idxk], z_ [idxk] ]
      exp4 = sum_ [(idxk)] exp3
      exp5 = add' [ sum_ [idxn] (mul [exp2,  exp4]), varx ] 

  printf "exp5 = %s\n"  ((prettyPrint . exp2RExp) exp5 :: String)
  putStrLn "\n---------------------------------------\n"
  
  cPrint "testfunction" [V (mkSym "x") [], V (mkSym "y") [idxi,idxj], V (mkSym "z") [idxi] ] exp5


test14 :: IO ()
test14 = do
  let idxset = [idxi,idxj,idxk]
      idxset2 = [idxm,idxn]
  putStrLn "idxset = (i,1,2),(j,1,2),(k,1,2), idxset2 = (m,1,2),(n,1,2)"
  putStrLn "------------------------------"
  putStrLn $ "sizeIndex idxset =" ++ show (sizeIndex idxset)
  putStrLn $ "sizeIndex idxset2 = " ++ show (sizeIndex idxset2)
  
  putStrLn $ "indexFlatteningFactors idxset = " ++ show (indexFlatteningFactors idxset)
  putStrLn $ "indexFlatteningFactors idxset2 = " ++ show (indexFlatteningFactors idxset2)
  putStrLn "------------------------------"
  putStrLn $ "flatIndex idxset [2,2,1] = " ++ show (flatIndex idxset [2,2,1])
  putStrLn $ "splitIndex idxset 6 = " ++ show (splitIndex idxset 6)
  putStrLn $ "flatIndexDisjoint [idxset,idxset2] (L [2,2,1]) = " ++ show (flatIndexDisjoint [idxset,idxset2] (L [2,2,1]))
  putStrLn $ "flatIndexDisjoint [idxset,idxset2] (R (L [2,2]))) = " ++ show (flatIndexDisjoint [idxset,idxset2] (R (L [2,2])))
  putStrLn $ "splitIndexDisjoint [idxset,idxset2] 6) = " ++ show (splitIndexDisjoint [idxset,idxset2] 6)
  putStrLn $ "splitIndexDisjoint [idxset,idxset2] 11) = " ++ show (splitIndexDisjoint [idxset,idxset2] 11)

test15 :: IO ()
test15 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let exp1 :: MExp Int
      exp1 = concat_ idxI [ x_ [idxi], y_ [idxj] ]
  let xvals = VS.fromList [101,102]
      yvals = VS.fromList [203,204]
      args = Args (HM.fromList [(mkSym "x",xvals),(mkSym "y",yvals)])
      
  forM_ [1,2,3,4] $ \i -> do
    let iptI = [("I",i)]
    printf "val(I=%d) = %d \n" i (seval args iptI exp1)

test16 :: IO ()
test16 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let exp1 :: MExp Int
      exp1 = concat_ idxI [ x_ [idxi], y_ [idxj] ]
  let exp2 :: MExp Int
      exp2 = mul [ cdelta idxI [[idxi],[idxj]] 2, exp1 ] 
  let xvals = VS.fromList [101,102]
      yvals = VS.fromList [203,204]
      args = Args (HM.fromList [(mkSym "x",xvals),(mkSym "y",yvals)])
  prettyPrintR exp2
  -- digraph exp

  forM_ [(i,j) | i <- [1,2,3,4], j <- [1,2] ] $ \(i,j) -> do
    let iptI = [("I",i)]
        iptj = [("j",j)]
    printf "val(I=%d,j=%d) = %d \n" i j (seval args (iptI++iptj) exp2)


test17 :: IO ()
test17 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let exp1 :: MExp Int
      exp1 = concat_ idxI [ mul [ x_ [idxi], x_ [idxi] ]  , mul [ y_ [idxj], x_ [idxj] ] ]

      exp' = sdiff (V (mkSym "x") [idxk]) exp1
  putStr "f = "
  prettyPrintR exp1
  putStr "df/dx_k = "
  prettyPrintR exp'

  let xvals = VS.fromList [101,102]
      yvals = VS.fromList [203,204]
      args = Args (HM.fromList [(mkSym "x",xvals),(mkSym "y",yvals)])
  
  forM_ [(iI,k) | iI <- [1,2,3,4], k <- [1,2] ] $ \(iI,k) -> do
    let iptI = [("I",iI)]
        iptk = [("k",k)]
    printf "val(I=%d,k=%d) = %d \n" iI k (seval args (iptI++iptk) exp')
  
    
