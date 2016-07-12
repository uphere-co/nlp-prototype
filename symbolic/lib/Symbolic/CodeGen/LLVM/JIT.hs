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
      r <- runExceptT $ withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s
          
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mfn <- EE.getFunction ee (AST.Name "main")
            action mfn
      case r of
        Left err -> putStrLn err >> return r
        Right _ -> return r

