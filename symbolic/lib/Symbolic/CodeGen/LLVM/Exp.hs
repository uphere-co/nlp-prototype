{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen.LLVM.Exp where

import Control.Monad.State
import Control.Applicative

import Data.Foldable ( foldrM )
import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map


import LLVM.General.AST ( Type(..), Operand(..), Instruction(..), Named (..)
                        , Terminator(..), Definition(..)
                        , FastMathFlags(..), Module (..)
                        , FloatingPointFormat(..)
                        , defaultModule
                        )
import           LLVM.General.AST.Global
import qualified LLVM.General.AST                        as AST
import qualified LLVM.General.AST.Attribute              as A
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.Float                  as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate       as IP
import           LLVM.General.AST.Type                           ( double, i64, ptr)
import qualified LLVM.General.AST.Type                   as T    ( void ) 
-----

import           Control.Lens                    (view, _1)
-- import           Control.Monad.Trans.State
import           Data.Array                      ((!))
import qualified Data.Array                as A
import           Data.Graph                      (topSort)
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet              as HS
import           Data.List                       (foldl')
import           Data.MemoTrie
import           Language.C.Data
import           Language.C.Data.Ident
import           Language.C.Data.Position
import           Language.C.Pretty
import           Language.C.Syntax
import           Text.Printf
import           Text.PrettyPrint hiding (double)
--
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.Predefined hiding (add)
import           Symbolic.Print
import           Symbolic.Type
import qualified Symbolic.Type as S ( Exp(..))
--
import           Debug.Trace

-----------------------

uncurry3 f (a,b,c) = f a b c 
  
hVar h = printf "x%x" h

mkAssign name val = assign name val >> return ()

cgen4Const name = mkAssign name . fval

mkOp op h val = getvar (hVar h) >>= op val

mkAdd = mkOp fadd
mkMul = mkOp fmul

getElem ty s idx = 
  let arr = LocalReference (ptr ty) (AST.Name s)
  in load =<< getElementPtr arr [idx]

getIndex is = do
  let factors = scanr (*) 1 (tail (map (\(i,s,e) -> e-s+1)  is ) ++ [1])
  indices <- forM is $ \(i,s,_) -> do
    xref <- getvar i
    x <- load xref
    if s == 0
      then return x
      else isub x (ival s)
  (i1:irest) <- zipWithM (\x y -> if y == 1 then return x else imul x (ival y))
                  indices factors 
  foldrM iadd i1 irest       

     
cgen4fold name op ini [] = cgen4Const name ini
cgen4fold name op ini (h:hs) = do
  val1 <- getvar (hVar h)
  v' <- foldrM op val1 hs
  assign name v'
  return ()

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
  ifthen <- getBlock
  --
  setBlock ifelse
  flval <- fl
  br ifexit
  ifelse <- getBlock
  --
  setBlock ifexit
  phi double [(trval,ifthen), (flval,ifelse)]

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

mkInnerbody v = do
  mapM_ (\e -> llvmCodegen (hVar (getMHash e)) e) $ es_ordered
  llvmCodegen (hVar h_result) v
 where 
  h_result = getMHash v
  bmap = HM.insert h_result v (mexpMap v)
  (hashmap,table,depgraph) = mkDepGraphNoSum v
  hs_ordered = delete h_result (reverse (map (\i -> table ! i) (topSort depgraph)))
  es_ordered = map (flip justLookup bmap) hs_ordered
  
llvmCodegen :: (?expHash :: Exp Double :->: Hash)=> String -> MExp Double -> Codegen ()
llvmCodegen name (mexpExp -> Zero)          = cgen4Const name 0
llvmCodegen name (mexpExp -> One)           = cgen4Const name 1
llvmCodegen name (mexpExp -> Delta idxi idxj)     = do
  let ni = view _1 idxi
      nj = view _1 idxj
  i <- getvar ni >>= load
  j <- getvar nj >>= load
  x <- cgencond ("delta"++ni++nj) (icmp IP.EQ i j) (return fone) (return fzero)
  assign name x
  return () 
llvmCodegen name (mexpExp -> Var (Simple s))= mkAssign name (local (AST.Name s))
llvmCodegen name (mexpExp -> Var (Indexed s is)) = getIndex is >>= getElem double s >>= mkAssign name
llvmCodegen name (mexpExp -> Val n)         = cgen4Const name n
llvmCodegen name (mexpExp -> S.Add hs)      = cgen4fold name mkAdd 0 hs 
llvmCodegen name (mexpExp -> S.Mul hs)      = cgen4fold name mkMul 1 hs 
llvmCodegen name (mexpExp -> Fun sym hs)    = do
  lst <- mapM (getvar . hVar) hs
  val <- call (externf (AST.Name sym)) lst
  assign name val
  return ()
llvmCodegen name (MExp (Sum is h1) m i)     = do
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

        
llvmAST :: (?expHash :: Exp Double :->: Hash) => String -> [Symbol] -> MExp Double -> LLVM ()
llvmAST name syms v = define T.void name symsllvm $ do
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
                                  theindex <- getIndex is
                                  mkInnerbody v
                                  val <- getvar (hVar (getMHash v))
                                  p <- getElementPtr rref [theindex]
                                  store p val
                                  return ()
                            foldr (.) id (map mkFor is) innerstmt
                            ret_
  where mkarg (Simple v) = (double,AST.Name v)
        mkarg (Indexed v _) = (ptr double,AST.Name v)
        symsllvm = (ptr double, AST.Name "result") : (map mkarg syms)
