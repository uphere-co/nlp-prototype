{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Hashable
import           Data.MemoTrie

import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Float    as F
import qualified LLVM.General.AST.Constant as C
import           LLVM.General.AST.Type            ( double, i64 )

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

exp3 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp3 = add [ x , delta "i" "j", delta "k" "l" ] 

exp4 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp4 = add [ one , y_ [("i",1,2)] ] --  add [ y_ [("i",1,2)], one ]

main = do
  let ?expHash = trie hash
  -- putStr "pow(10,x) = "
  prettyPrintR exp4
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [Indexed "y" [("i",1,2)] ] exp4
              external double "sin" [(double, AST.Name "x")] 
              define double "main" [] $ do
                yref <- alloca (arrtype double 10)
                ptr <- getElementPtr yref [ izero, ione ] 
                store ptr fone
                
                res <- call (externf (AST.Name "fun1")) [ ione, yref ]

                  -- cons (C.Float (F.Double 10)) ]
                ret res 
  runJIT ast
  return ast
