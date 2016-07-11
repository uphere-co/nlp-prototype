{-# LANGUAGE ForeignFunctionInterface #-}

module Symbolic.CodeGen.LLVM.JIT where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, Ptr, castFunPtr )

import Control.Monad.Trans.Except

import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis

import qualified LLVM.General.ExecutionEngine as EE

type JITFunction = Ptr Double -> Ptr (Ptr Double) -> IO ()


foreign import ccall "dynamic" haskFun :: FunPtr JITFunction -> JITFunction 

run :: FunPtr a -> JITFunction
run fn = haskFun (castFunPtr fn :: FunPtr JITFunction)

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> (Maybe (FunPtr ()) -> IO b) -> IO (Either String b)

                                                             -- Maybe (FunPtr ())))
runJIT mod action = do
  withContext $ \context ->
    jit context $ \executionEngine -> do
      runExceptT $ withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          -- runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s
          
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mfn <- EE.getFunction ee (AST.Name "main")
            action mfn
          -- return (optmod,mfn) 

          {-
            case mainfn of
              Just fn -> do
                Alloc.alloca $ \pres -> 
                  Alloc.alloca $ \pargs -> 
                    Array.withArray [100,200,300,400,500,600,700,800,900,1000] $ \px -> do
                      poke pargs px
                      -- poke p 9.0
                      -- res <-
                      run fn pres pargs
                      res <- peek pres
                      putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()
          -}
          -- Return the optimized module
          -- return optmod 
      --print r
      -- return r
