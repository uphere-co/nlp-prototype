{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

module Symbolic.CodeGen.LLVM.Run where

import           Control.Monad.IO.Class           ( MonadIO(liftIO) )
import           Control.Monad.Reader.Class       ( MonadReader(ask) )
import           Control.Monad.Trans.Reader       ( ReaderT(..) )
import           Data.MemoTrie                    ( (:->:) )
import qualified Data.Vector.Storable      as VS
import           Foreign.ForeignPtr               ( ForeignPtr, withForeignPtr )
import           Foreign.Ptr                      ( Ptr )
import qualified LLVM.General.AST          as AST
import           LLVM.General.AST.Type            ( double, float, ptr, void )
import           LLVM.General.Context             ( withContext )
import qualified LLVM.General.ExecutionEngine as EE
import           LLVM.General.Module         as Mod
--
import           Symbolic.CodeGen.LLVM.Exp
import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.Type

initModule :: AST.Module
initModule = emptyModule "my cool jit"

mkArgRef :: Int -> a -> Codegen AST.Operand
mkArgRef i _ = getElem (ptr float) "args" (ival i)

mkASTWithExt :: (?expHash :: Exp Float :->: Hash) =>
                LLVM ()
             -> MExp Float
             -> [Variable]
             -> AST.Module
mkASTWithExt ext e args =
  runLLVM initModule $ do
    ext
    llvmAST "fun1" args e
    define void "main" [ (ptr float, AST.Name "res")
                       , (ptr (ptr float), AST.Name "args")
                       ] $ do
      argrefs <- mapM (uncurry mkArgRef) (zip [0..] args)
      call (externf (AST.Name "fun1")) (local (AST.Name "res") : argrefs)
      ret_



mkAST :: (?expHash :: Exp Float :->: Hash) =>
         MExp Float
      -> [Variable]
      -> AST.Module
mkAST = mkASTWithExt (return ())




unsafeWiths :: VS.Storable a => [VS.Vector a] -> ([Ptr a] -> IO b) -> IO b
unsafeWiths vs = go vs id
  where go []     ps f = f (ps [])
        go (x:xs) ps f = VS.unsafeWith x $ \p -> go xs (ps . (p:)) f



runMain :: [VS.Vector Float] -> ForeignPtr Float -> LLVMRun2T IO ()
runMain vargs fpr = do
  fn <- ask 
  liftIO  . unsafeWiths vargs $ \ps -> do
    let vps = VS.fromList ps
    VS.MVector _ fparg <- VS.thaw vps
    withForeignPtr fparg $ \pargs ->
      withForeignPtr fpr $ \pres ->
        run fn pres pargs

runJITASTPrinter :: (VS.Vector Float -> IO ())
                 -> AST.Module
                 -> [VS.Vector Float]
                 -> VS.Vector Float
                 -> LLVMContextT IO (Either String ())
runJITASTPrinter printer ast vargs vres =
  runJIT ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        unsafeWiths vargs $ \ps -> do
          let vps = VS.fromList ps
          VS.MVector _ fparg <- VS.thaw vps
          mv@(VS.MVector _ fpr) <- VS.thaw vres
          withForeignPtr fparg $ \pargs ->
            withForeignPtr fpr $ \pres -> do
              run fn pres pargs
              vr' <- VS.freeze mv
              printer vr'
