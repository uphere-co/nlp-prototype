{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Hashable
import           Data.MemoTrie

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.Constant as C

import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Lang
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type

initModule :: AST.Module
initModule = emptyModule "my cool jit"

exp1 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp1 = mul [val 1,val 3] -- val 3 -- zero

exp2 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp2 = power 10 x -- power 10 (x `add'` y)

main = do
  let ?expHash = trie hash
  putStr "pow(10,x) = "
  prettyPrintR exp2 
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [Simple "x"] exp2
              -- external double "sin" [(double, AST.Name "x")] 
              define double "main" [] $ do
                res <- call (externf (AST.Name "fun1")) [ cons (C.Float (F.Double 10)) ]
                ret res 
  runJIT ast
  return ast
