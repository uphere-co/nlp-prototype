{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

module Symbolic.CodeGen.LLVM.Run where

import           Data.MemoTrie                    ( (:->:) )
import qualified Data.Vector.Storable      as VS
import           Foreign.ForeignPtr               ( withForeignPtr )
import           Foreign.Ptr                      ( Ptr )
import qualified LLVM.General.AST          as AST
import           LLVM.General.AST.Type            ( double, ptr, void )
--
import           Symbolic.CodeGen.LLVM.Exp
import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.Type

initModule :: AST.Module
initModule = emptyModule "my cool jit"

mkArgRef :: Int -> a -> Codegen AST.Operand
mkArgRef i _ = getElem (ptr double) "args" (ival i)

mkAST :: (?expHash :: Exp Double :->: Hash) =>
         MExp Double -> [Variable] -> AST.Module
mkAST e args =
  runLLVM initModule $ do
    llvmAST "fun1" args e
    define void "main" [ (ptr double, AST.Name "res")
                       , (ptr (ptr double), AST.Name "args")
                       ] $ do
      argrefs <- mapM (uncurry mkArgRef) (zip [0..] args)
      call (externf (AST.Name "fun1")) (local (AST.Name "res") : argrefs)
      ret_

unsafeWiths :: VS.Storable a => [VS.Vector a] -> ([Ptr a] -> IO b) -> IO b
unsafeWiths vs = go vs id
  where go []     ps f = f (ps [])
        go (x:xs) ps f = VS.unsafeWith x $ \p -> go xs (ps . (p:)) f


runJITASTPrinter :: (VS.Vector Double -> IO ())
                 -> AST.Module
                 -> [VS.Vector Double]
                 -> VS.Vector Double
                 -> IO (Either String ())
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
