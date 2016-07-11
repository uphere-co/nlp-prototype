{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Hashable
import           Data.MemoTrie

import qualified LLVM.General.AST          as AST
import qualified LLVM.General.AST.Float    as F
import qualified LLVM.General.AST.Constant as C
import           LLVM.General.AST.Type            ( double, i64, ptr )

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
exp5 = sum_ [idxi, idxj] (add [ y_ [idxi,idxj], one ] )
  where idxi = ("i",0,2)
        idxj = ("j",0,2)
-- add [ zero , y_ [("i",0,3),("j",1,2)] ] 

exp6 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
exp6 = sum_ [("i",0,9)] (fun "sin" [ y_ [("i",0,9)] ])

test1 = do
  let ?expHash = trie hash
  -- putStr "pow(10,x) = "
  prettyPrintR exp5
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [Indexed "y" [("i",0,2),("j",0,2)] ] exp5
              external double "sin" [(double, AST.Name "x")] 
              define double "main" [] $ do
                yref <- alloca (arrtype double 10)
                let setarr arr (n,v) = do
                      ptr <- getElementPtr arr [ ival 0, ival n ]
                      store ptr (fval v)
                mapM_ (setarr yref) $ zip [0..9] [1,2,3,4,5,6,7,8,9,10]

                
                res <- call (externf (AST.Name "fun1")) [ yref ]

                ret res 
  runJIT ast
  return ast

test2 = do
  let ?expHash = trie hash
  prettyPrintR exp5
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [ Indexed "y" [("i",0,2),("j",0,2)] ] exp5
              external double "sin" [(double, AST.Name "x")] 
             
              define double "main" [(ptr double, AST.Name "x")] $ do
                res <- call (externf (AST.Name "fun1")) [ local (AST.Name "x") ]
                ret res 
  runJIT ast
  return ast


main = test2
