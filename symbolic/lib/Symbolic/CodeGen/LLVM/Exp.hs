{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen.LLVM.Exp where

import           Control.Lens                             (view, _1)
import           Control.Monad.State
import           Data.Array                               ((!))
import           Data.Foldable                            (foldrM)
import           Data.Graph                               (topSort)
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Data.List
import           Data.MemoTrie
import           LLVM.General.AST ( Operand(..) )
import qualified LLVM.General.AST                  as AST
import qualified LLVM.General.AST.IntegerPredicate as IP
import           LLVM.General.AST.Type                    (double, i64, ptr)
import qualified LLVM.General.AST.Type             as T   (void) 
import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.Type
import qualified Symbolic.Type                     as S   (Exp(..))
import           Symbolic.Util                            (indexFlatteningFactors)
--

hVar :: Int -> String
hVar h = printf "x%x" h

-- mkAssign :: String -> Operand -> Codegen ()
-- mkAssign name val = assign name val >> return ()

-- cgen4Const :: String -> Double -> Codegen ()
-- cgen4Const name = mkAssign name . fval

mkOp :: (Operand -> Operand -> Codegen Operand) -> Int -> Operand -> Codegen Operand
mkOp op h val = getvar (hVar h) >>= op val

mkAdd :: Int -> Operand -> Codegen Operand
mkAdd = mkOp fadd

mkMul :: Int -> Operand -> Codegen Operand
mkMul = mkOp fmul

getElem :: AST.Type -> String -> Operand -> Codegen Operand
getElem ty s i = 
  let arr = LocalReference (ptr ty) (AST.Name s)
  in load =<< getElementPtr arr [i]

loadIndices :: [Index] -> Codegen [Operand]
loadIndices = mapM (load <=< getvar . indexName) 

flattenByM :: [Operand] -> [Int] -> Codegen Operand
flattenByM is fac = do
  (i1:irest) <- zipWithM (\x y -> if y == 1 then return x else imul x (ival y))
                  is fac 
  foldrM iadd i1 irest       

flatIndexM :: [Index] -> [Operand] -> Codegen Operand
flatIndexM is ivs = do
  let factors = indexFlatteningFactors is
  indices <- zipWithM index0baseM is ivs
  flattenByM indices factors

index0baseM :: Index -> Operand -> Codegen Operand
index0baseM (i,s,_) x = if s == 0 then return x else isub x (ival s)

cgen4fold :: String -> (Int -> Operand -> Codegen Operand) -> Double -> [Int] -> Codegen Operand
cgen4fold name _  ini []     = assign name (fval ini)
cgen4fold name op _   (h:hs) = do
  val1 <- getvar (hVar h)
  v' <- foldrM op val1 hs
  assign name v'

cgencond :: String -> Codegen Operand -> Codegen Operand -> Codegen Operand -> Codegen Operand
cgencond label cond tr fl = do
  ifthen <- addBlock (label ++ ".then")
  ifelse <- addBlock (label ++ ".else")
  ifexit <- addBlock (label ++ ".exit")
  --
  condval <- cond
  cbr condval ifthen ifelse
  --
  setBlock ifthen
  trval <- tr
  br ifexit
  ifthen' <- getBlock
  --
  setBlock ifelse
  flval <- fl
  br ifexit
  ifelse' <- getBlock
  --
  setBlock ifexit
  phi double [(trval,ifthen'), (flval,ifelse')]

cgenfor :: String -> Index -> Codegen () -> Codegen ()
cgenfor label (ivar,start,end) body = do
  forloop <- addBlock (label ++ ".loop")
  forexit <- addBlock (label ++ ".exit")
  --
  iref <- alloca i64
  store iref (ival start)
  assign ivar iref
  br forloop
  --
  setBlock forloop
  body
  i <- load iref
  i' <- iadd i (ival 1)
  store iref i'
  --
  test <- icmp IP.ULE i' (ival end)
  cbr test forloop forexit
  --
  setBlock forexit
  return ()  

mkInnerbody :: (?expHash :: Exp Double :->: Hash) => MExp Double -> Codegen Operand
mkInnerbody v = do
  mapM_ (\e -> llvmCodegen (hVar (getMHash e)) e) $ es_ordered
  llvmCodegen (hVar h_result) v
 where 
  h_result = getMHash v
  bmap = HM.insert h_result v (mexpMap v)
  (_hashmap,table,depgraph) = mkDepGraphNoSum v
  hs_ordered = delete h_result (reverse (map (\i -> table ! i) (topSort depgraph)))
  es_ordered = map (flip justLookup bmap) hs_ordered
  
llvmCodegen :: (?expHash :: Exp Double :->: Hash) =>
               String -> MExp Double -> Codegen Operand
llvmCodegen name (MExp Zero _ _)                 = assign name (fval 0)
llvmCodegen name (MExp One _ _)                  = assign name (fval 1)
llvmCodegen name (MExp (Delta idxi idxj) _ _)    = do
  let ni = indexName idxi
      nj = indexName idxj
  i <- getvar ni >>= load
  j <- getvar nj >>= load
  x <- cgencond ("delta"++ni++nj) (icmp IP.EQ i j) (return fone) (return fzero)
  assign name x
llvmCodegen name (MExp (CDelta _ _ _) _ _) = error "CDelta not implemented"
llvmCodegen name (MExp (Var (Simple s)) _ _)     = assign name (local (AST.Name s))
llvmCodegen name (MExp (Var (Indexed s is)) _ _) = 
  loadIndices is >>= flatIndexM is >>= getElem double s >>= assign name
llvmCodegen name (MExp (Val n) _ _)              = assign name (fval n)
llvmCodegen name (MExp (S.Add hs) _ _)           = cgen4fold name mkAdd 0 hs 
llvmCodegen name (MExp (S.Mul hs) _ _)           = cgen4fold name mkMul 1 hs 
llvmCodegen name (MExp (Fun sym hs) _ _)         = do
  lst <- mapM (getvar . hVar) hs
  val <- call (externf (AST.Name sym)) lst
  assign name val
llvmCodegen name (MExp (Sum is h1) m _)          = do
  sumref <- alloca double
  store sumref (fval 0)
  let mkFor = \(i,s,e) -> cgenfor ("for_" ++ i) (i,s,e)
      innerstmt = do
        mkInnerbody (justLookup h1 m)
        s <- load sumref
        v <- getvar (hVar h1)
        s' <- fadd s v 
        store sumref s'
        return ()
  foldr (.) id (map mkFor is) innerstmt
  rval <- load sumref
  assign name rval
llvmCodegen name (MExp (Concat i hs) m is)    = do

  error "llvmCodegen: Concat not implemented"

        
llvmAST :: (?expHash :: Exp Double :->: Hash) =>
           String -> [Symbol] -> MExp Double -> LLVM ()
llvmAST name syms v =
  define T.void name symsllvm $ do
    let rref = LocalReference (ptr double) (AST.Name "result")
        is = HS.toList (mexpIdx v)
    if null is
      then do
        mkInnerbody v
        val <- getvar (hVar (getMHash v))
        store rref val
        ret_
      else do
        let mkFor = \(i,s,e) -> cgenfor ("for_" ++ i) (i,s,e)
            innerstmt = do
              theindex <- flatIndexM is =<< loadIndices is
              mkInnerbody v
              val <- getvar (hVar (getMHash v))
              p <- getElementPtr rref [theindex]
              store p val
              return ()
        foldr (.) id (map mkFor is) innerstmt
        ret_
  where
    mkarg (Simple n) = (double,AST.Name n)
    mkarg (Indexed n _) = (ptr double,AST.Name n)
    symsllvm = (ptr double, AST.Name "result") : (map mkarg syms)
