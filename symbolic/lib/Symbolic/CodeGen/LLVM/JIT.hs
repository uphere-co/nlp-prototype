{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Symbolic.CodeGen.LLVM.JIT where

import           Control.Monad.IO.Class             ( MonadIO(liftIO) )
import           Control.Monad.Reader.Class         ( MonadReader(ask) )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader         ( ReaderT(..) )
import           Foreign.Ptr                        (FunPtr, Ptr, castFunPtr)
import qualified LLVM.General.AST            as AST
import           LLVM.General.Context
import qualified LLVM.General.ExecutionEngine as EE
import           LLVM.General.Module         as Mod
import           LLVM.General.PassManager

type JITFunction = Ptr Float -> Ptr (Ptr Float) -> IO ()


foreign import ccall "dynamic" haskFun :: FunPtr JITFunction -> JITFunction 

{- 
newtype LLVMRunT m a = LLVMRunT { runLLVMRunT :: ReaderT Context m a }

deriving instance (Functor m) => Functor (LLVMRunT m)
deriving instance (Applicative m) => Applicative (LLVMRunT m)
deriving instance (Monad m) => Monad (LLVMRunT m)
deriving instance (Monad m) => MonadReader Context (LLVMRunT m)
deriving instance (MonadIO m) => MonadIO (LLVMRunT m)
-}

type LLVMRunT m = ReaderT Context m

runLLVMRunT = runReaderT

run :: FunPtr a -> JITFunction
run fn = haskFun (castFunPtr fn :: FunPtr JITFunction)

jit :: (Context -> EE.MCJIT -> IO a) -> LLVMRunT IO a
jit action = do c <- ask 
                liftIO $ EE.withMCJIT c optlevel model ptrelim fastins (action c)
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module
       -> (Maybe (FunPtr ()) -> IO b)
       -> LLVMRunT IO (Either String b)
runJIT mod' action = do
  jit $ \context executionEngine -> do
      r <- runExceptT $ withModuleFromAST context mod' $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          -- runPassManager pm m
          _optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s
          
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mfn <- EE.getFunction ee (AST.Name "main")
            action mfn
      case r of
        Left err -> putStrLn err >> return r
        Right _ -> return r

type LLVMRun2T = ReaderT (FunPtr ()) 

runJIT2 :: AST.Module -> LLVMRun2T IO b -> LLVMRunT IO (Either String b)
runJIT2 mod' action = do
  jit $ \context executionEngine -> do
      runExceptT $ withModuleFromAST context mod' $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          _optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s
          
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mfn <- EE.getFunction ee (AST.Name "main")
            case mfn of
              Nothing -> error "no main"
              Just fn -> runReaderT action fn
{-      case r of
        Left err -> putStrLn err >> return r
        Right _ -> return r
-}
