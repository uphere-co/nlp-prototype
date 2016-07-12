{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Concurrent
import           Data.Hashable
import           Data.MemoTrie
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.Marshal.Array as Array
import           Foreign.Storable (poke, peek, pokeElemOff)

import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Float    as F
import qualified LLVM.General.AST.Constant as C
import           LLVM.General.AST.Type            ( double, i64, ptr, void )
--
import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Lang
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type

initModule :: AST.Module
initModule = emptyModule "my cool jit"

exp1 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp1 = mul [val 1,val 3]

exp2 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp2 = power 10 x

exp3 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp3 = delta idxi idxj  --  add [ x , delta idxi idxj, delta idxk idxl ] 
  where idxi = ("i",0,2)
        idxj = ("j",0,2)
        idxk = ("k",0,2)
        idxl = ("l",0,2) 

exp4 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp4 = add [ zero , y_ [("i",0,3),("j",1,2)] ] 

exp5 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp5 = sum_ [idxi, idxj] (y_ [idxi,idxj])
  where idxi = ("i",0,2)
        idxj = ("j",0,2)

exp6 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp6 = sum_ [("i",0,9)] (fun "sin" [ y_ [("i",0,9)] ])

exp7 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp7 = add [ x, y ] 

test2 = do
  let ?expHash = trie hash
  prettyPrintR exp5
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [ Indexed "y" [("i",0,2),("j",0,2)] ] exp5
              external double "sin" [(double, AST.Name "x")] 
             
              define void "main" [ (ptr double, AST.Name "res")
                                 , (ptr (ptr double), AST.Name "args")
                                 ] $ do
                xref <- getElem (ptr double) "args" (ival 0)
                call (externf (AST.Name "fun1")) [ local (AST.Name "res"), xref ]
                -- store (local (AST.Name "res")) res
                ret_
  runJIT ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        Alloc.alloca $ \pres -> 
          Alloc.alloca $ \pargs -> 
            Array.withArray [100,200,300,400,500,600,700,800,900,1000] $ \px -> do
              poke pargs px
              run fn pres pargs
              res <- peek pres
              putStrLn $ "Evaluated to: " ++ show res
          

test3 = do
  let ?expHash = trie hash
  prettyPrintR exp7
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [ Simple "x", Simple "y" ] exp7
              external double "sin" [(double, AST.Name "x")] 
             
              define void "main" [ (ptr double, AST.Name "res")
                                 , (ptr (ptr double), AST.Name "args")
                                 ] $ do
                xref <- getElem (ptr double) "args" (ival 0)
                yref <- getElem (ptr double) "args" (ival 1)
                x <- load xref
                y <- load yref
                -- res <-
                call (externf (AST.Name "fun1")) [ local (AST.Name "res"), x, y ]
                -- store (local (AST.Name "res")) res
                ret_
  runJIT ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        forkOS $ do
          threadDelay 500000
          Alloc.alloca $ \pres -> 
            Alloc.alloca $ \pargs -> 
              Alloc.alloca $ \px ->
                Alloc.alloca $ \py -> do
                  poke px 700
                  poke py 300
                  pokeElemOff pargs 0 px
                  pokeElemOff pargs 1 py
                  run fn pres pargs
                  res <- peek pres
                  putStrLn $ "Evaluated to: " ++ show res
        do
          threadDelay 1000000
          Alloc.alloca $ \pres -> 
            Alloc.alloca $ \pargs -> 
              Alloc.alloca $ \px ->
                Alloc.alloca $ \py -> do
                  poke px 500
                  poke py 900
                  pokeElemOff pargs 0 px
                  pokeElemOff pargs 1 py
                  run fn pres pargs
                  res <- peek pres
                  putStrLn $ "Evaluated to: " ++ show res



test4 = do
  let ?expHash = trie hash
  let exp = exp3
  prettyPrintR (exp :: MExp Double)
  -- print (mexpIdx exp)
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [] exp
              define void "main" [ (ptr double, AST.Name "res")
                                 , (ptr (ptr double), AST.Name "args")
                                 ] $ do
                xref <- getElem (ptr double) "args" (ival 0)
                call (externf (AST.Name "fun1")) [ local (AST.Name "res"), xref ]
                -- store (local (AST.Name "res")) res
                ret_
  runJIT ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        Array.allocaArray 9 $ \pres -> 
          Alloc.alloca $ \pargs -> do
            run fn pres pargs
            res <- Array.peekArray 9 pres
            putStrLn $ "Evaluated to: " ++ show res

                
  {- runJIT ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?" -}

main = test4
