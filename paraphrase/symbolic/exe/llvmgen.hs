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
exp4 = add [ zero , y_ [("i",0,3),("j",1,2)] ] 

exp5 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp5 = add [ sum_ [("i",0,3)] zero , zero ]

-- add [ zero , y_ [("i",0,3),("j",1,2)] ] 


main' = do
  let ?expHash = trie hash
  -- putStr "pow(10,x) = "
  prettyPrintR exp4
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [Indexed "y" [("i",1,10)] ] exp4
              external double "sin" [(double, AST.Name "x")] 
              define double "main" [] $ do
                yref <- alloca (arrtype double 10)
                let setarr arr (n,v) = do
                      ptr <- getElementPtr arr [ ival 0, ival n ]
                      store ptr (fval v)
                mapM_ (setarr yref) $ zip [0..9] [1,2,3,4,5,6,7,8,9,10]

                
                res <- call (externf (AST.Name "fun1")) [ ival 2, ival 2, yref ]

                ret res 
  runJIT ast
  return ast

main = do
  let ?expHash = trie hash
  -- putStr "pow(10,x) = "
  prettyPrintR exp5
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [] exp5
              define double "main" [] $ do
{-                 yref <- alloca (arrtype double 10)
                let setarr arr (n,v) = do
                      ptr <- getElementPtr arr [ ival 0, ival n ]
                      store ptr (fval v)
                mapM_ (setarr yref) $ zip [0..9] [1,2,3,4,5,6,7,8,9,10] -}

                
                res <- call (externf (AST.Name "fun1")) [ ]

                ret res 
  runJIT ast
  return ast

