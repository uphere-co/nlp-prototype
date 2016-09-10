{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test.LLVM where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Foldable             (forM_)
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.MemoTrie
import qualified Data.Vector.Storable as VS
import           Text.Printf
--
-- import           Symbolic.CodeGen.C
import           Symbolic.CodeGen.LLVM.Exp
import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.CodeGen.LLVM.Run
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Simplify
import           Symbolic.Type
import           Symbolic.Util
--

delta_test :: LLVMContextT IO () 
delta_test = do
  let ?expHash = trie hash
      ?functionMap = HM.empty

  let v_wd  = tV [0.1,0.2,0.3,0.4,0.3,0.2,0.1,0.2]
      v_y   = tV [1.3, 2.5]
      v_bd  = tV [1.2,1.1,0.9,0.8]
      args0 = mkA [ ("wd",v_wd), ("y" ,v_y), ("bd",v_bd) ]
      n = 2
      m = 2*n
      idxm = ("m",1,n)
      idxJ = ("J",1,2*n)
      idxk = ("k",1,n)
      idxI = ("I",1,2*n)
      --
      (y,wd,bd) = (iV ("y",[idxk]),iV ("wd",[idxI,idxk]),iV ("bd",[idxI]))
      --
      prd = sum_ [idxk] (mul [wd, y])
      result = prd -- add [prd, bd] -- tanh_ [ add [prd, bd] ]
      dresult = sdiff HM.empty (mkV ("wd",[idxJ,idxm])) result
      
  liftIO $ printf "result = %s\n"  ((prettyPrint . exp2RExp) result :: String)
  liftIO $ printf "dresult = %s\n"  ((prettyPrint . exp2RExp) dresult :: String)

  forM_ [1..2*n] $ \iI -> do
    let iptI = [("I",iI)]
        r0 = seval args0 iptI result
    liftIO $ printf "I=%d, r0 = %f \n" iI r0

  forM_ [(iI,iJ,k)| k <- [1..n], iJ <- [1..m], iI <- [1..m] ] $ \(iI,iJ,k) -> do
    let ipt = [("I",iI),("J",iJ),("m",k)]
    liftIO $ printf "diff(I=%d,J=%d,m=%d) = %f \n" iI iJ k (seval args0 ipt dresult)

  liftIO $ putStrLn "LLVM code result:"
  let arglst = map mkV [("y",[idxk]),("wd",[idxI,idxk]),("bd",[idxI])]
  let fullAST = mkASTWithExt (return ()) [ ("result",(result,arglst)), ("dresult",(dresult,arglst)) ]
  
  compileNRun ["result","dresult"] fullAST $ do
    r <- mutateWith (VS.replicate m 0) $ \fpr ->
      callFn "result" [v_y,v_wd,v_bd] fpr
    liftIO $ print r
    dr <- mutateWith (VS.replicate (m*m*n) 0) $ \fpr -> 
      callFn "dresult" [v_y,v_wd,v_bd] fpr
    liftIO $ print dr
  return ()
