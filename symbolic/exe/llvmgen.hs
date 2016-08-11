{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Concurrent
import           Data.Foldable                  ( forM_ )
import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.MemoTrie
import qualified Data.Vector.Storable  as VS
import           Foreign.ForeignPtr             ( withForeignPtr )
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.Marshal.Array as Array
import           Foreign.Storable               ( poke, peek, pokeElemOff )

import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Float    as F
import qualified LLVM.General.AST.Constant as C
import           LLVM.General.AST.Type            ( double, i64, ptr, void )
import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.Exp
import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type

initModule :: AST.Module
initModule = emptyModule "my cool jit"

mkArgRef i _ = getElem (ptr double) "args" (ival i)

mkAST exp args =
  runLLVM initModule $ do
    llvmAST "fun1" args exp
    define void "main" [ (ptr double, AST.Name "res")
                       , (ptr (ptr double), AST.Name "args")
                       ] $ do
      argrefs <- mapM (uncurry mkArgRef) (zip [0..] args)
      call (externf (AST.Name "fun1")) (local (AST.Name "res") : argrefs)
      ret_

unsafeWiths vs f = go vs id f
  where go []     ps f = f (ps [])
        go (v:vs) ps f = VS.unsafeWith v $ \p -> go vs (ps . (p:)) f



runJITASTPrinter printer ast vargs vres =
  runJIT ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        unsafeWiths vargs $ \ps -> do
          let vps = VS.fromList ps
          mvarg@(VS.MVector _ fparg) <- VS.thaw vps
          mv@(VS.MVector _ fpr) <- VS.thaw vres
          withForeignPtr fparg $ \pargs ->
            withForeignPtr fpr $ \pres -> do
              run fn pres pargs
              vr' <- VS.freeze mv
              printer vr'



exp1 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp1 = mul [val 1,val 3]

exp2 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp2 = power 10 x

exp3 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp3 = delta idxi idxj 
  where idxi = ("i",0,2)
        idxj = ("j",0,2)
        idxk = ("k",0,2)
        idxl = ("l",0,2) 

exp4 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp4 = add [ zero , y_ [("i",0,3),("j",1,2)] ] 


exp6 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp6 = sum_ [("i",0,9)] (fun "sin" [ y_ [("i",0,9)] ])

exp7 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp7 = add [ x, y ] 

exp8 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp8 = sum_ [idxj] (mul [ x_ [idxi,idxj] , y_ [ idxj ] ] )
  where idxi = ("i",0,9)
        idxj = ("j",0,9)


test2 = do
  let ?expHash = trie hash
  let exp :: MExp Double
      exp = sum_ [idxi, idxj] (y_ [idxi,idxj])
      idxi = ("i",0,2)
      idxj = ("j",0,2)
  
  prettyPrintR exp
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [ Indexed "y" [idxi, idxj] ] exp
              external double "sin" [(double, AST.Name "x")] 
             
              define void "main" [ (ptr double, AST.Name "res")
                                 , (ptr (ptr double), AST.Name "args")
                                 ] $ do
                xref <- getElem (ptr double) "args" (ival 0)
                call (externf (AST.Name "fun1")) [ local (AST.Name "res"), xref ]
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
                call (externf (AST.Name "fun1")) [ local (AST.Name "res"), x, y ]
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
  let ast = mkAST exp []
  runJIT ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        Array.allocaArray 9 $ \pres -> 
          Alloc.alloca $ \pargs -> do
            run fn pres pargs
            res <- Array.peekArray 9 pres
            putStrLn $ "Evaluated to: " ++ show res

test5 = do
  let ?expHash = trie hash
  let exp = exp8
  prettyPrintR (exp :: MExp Double)
  let idxi = ("i",1,10)
      idxj = ("j",1,10)
  let ast = mkAST exp  [ Indexed "x" [idxi,idxj], Indexed "y" [idxj] ]
      vx = VS.fromList [1..100] :: VS.Vector Double
      vy = VS.fromList [1..10]  :: VS.Vector Double
      vr = VS.replicate 10 0    :: VS.Vector Double
  runJITASTPrinter (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr


test6 = do
  let ?expHash = trie hash
  let exp :: MExp Double
      exp = concat_ idxA [ x_ [idxi,idxj], y_ [idxk] ] 
      idxA = ("A",1,10)
      idxi = ("i",1,2)
      idxj = ("j",1,3)
      idxk = ("k",1,4)
  prettyPrintR (exp :: MExp Double)
  -- digraph exp
  let ast = mkAST exp [ Indexed "x" [idxi,idxj], Indexed "y" [idxk] ]
      vx  = VS.fromList [1,2,3,4,5,6]
      vy  = VS.fromList [11,12,13,14]  :: VS.Vector Double
      vr  = VS.replicate 10 0    :: VS.Vector Double
  runJITASTPrinter (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr

test7 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxI = ("I",1,4)
      exp1 :: MExp Double
      exp1 = concat_ idxI [ x_ [idxi], y_ [idxj] ]
  let exp :: MExp Double
      exp = mul [ cdelta idxI [[idxi],[idxj]] 2, exp1 ] 
  prettyPrintR exp
  -- digraph exp
  let ast = mkAST exp [ Indexed "x" [idxi], Indexed "y" [idxj] ]
      vx = VS.fromList [101,102]
      vy = VS.fromList [203,204] :: VS.Vector Double
      vr = VS.replicate 8 0    :: VS.Vector Double
  runJITASTPrinter (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr

  -- comparison
  let xvals = VS.fromList [101,102]
      yvals = VS.fromList [203,204]
      args = Args HM.empty (HM.fromList [("x",xvals),("y",yvals)])
  forM_ [(i,j) | i <- [1,2,3,4], j <- [1,2] ] $ \(i,j) -> do
    let iptI = [("I",i)]
        iptj = [("j",j)]
        
    printf "val(I=%d,j=%d) = %f \n" i j (seval args (iptI++iptj) exp)
  


test8 = do
  let idxi = ("i",1,2)
      idxj = ("j",1,2)

      idxI = ("I",1,4)
      idxk = ("k",1,2)
  
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let exp :: MExp Double
      exp = concat_ idxI [ mul [ x_ [idxi], x_ [idxi] ]  , mul [ y_ [idxj], x_ [idxj] ] ]

      exp' = sdiff (Indexed "x" [idxk]) exp
  putStr "f = "
  prettyPrintR exp
  putStr "df/dx_k = "
  prettyPrintR exp'
  let ast = mkAST exp [ Indexed "x" [idxi], Indexed "y" [idxj] ]
      vx = VS.fromList [101,102]
      vy = VS.fromList [203,204] :: VS.Vector Double
      vr = VS.replicate 8 0    :: VS.Vector Double
  runJITASTPrinter (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr
  
main = test7

