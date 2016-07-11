{-# LANGUAGE ForeignFunctionInterface #-}

module Symbolic.CodeGen.LLVM.JIT where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, Ptr, castFunPtr )
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.Marshal.Array as Array
import           Foreign.Storable (poke)

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

foreign import ccall "dynamic" haskFun :: FunPtr (Ptr Double -> IO Double) -> Ptr Double -> IO Double

run :: FunPtr a -> Ptr Double -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (Ptr Double -> IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod = do
  withContext $ \context ->
    jit context $ \executionEngine -> do
      r <- runExceptT $ withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          {-runPassManager pm m-}
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s
          
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case mainfn of
              Just fn -> do
                Array.withArray [100,200,300,400,500,600,700,800,900,1000] $ \p -> do
                  -- poke p 9.0
                  res <- run fn p
                  putStrLn $ "Evaluated to: " ++ show res
              Nothing -> return ()

          -- Return the optimized module
          return optmod 
      --print r
      return r
