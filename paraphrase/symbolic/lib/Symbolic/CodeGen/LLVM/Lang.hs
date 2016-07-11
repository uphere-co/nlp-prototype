{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen.LLVM.Lang where

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
import           Symbolic.Predefined hiding (add)
import           Symbolic.Print
import           Symbolic.Type
import qualified Symbolic.Type as S ( Exp(..))
--
import           Debug.Trace



-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> String -> [(Type, AST.Name)] -> Codegen a -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = AST.Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = bls
  }
  where
    bls = createBlocks $ execCodegen $ do
      enter <- addBlock entryBlockName
      void $ setBlock enter
      body

external ::  Type -> String -> [(Type, AST.Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = AST.Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
-- double :: Type
-- double = FloatingPointType 64 IEEE

-- int64 :: Type
-- int64 = Int64

arrtype :: Type -> Int -> Type 
arrtype typ n = ArrayType (fromIntegral n) typ

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

instance IsString AST.Name where
  fromString = AST.Name . fromString

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: AST.Name                     -- Name of the active block to append to
  , blocks       :: Map.Map AST.Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  -- , symval       :: SymbolTable              -- Function scope symbol-value table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (AST.Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (AST.Name entryBlockName) Map.empty [] {- [] -} 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (AST.UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen AST.Name
entry = gets currentBlock

addBlock :: String -> Codegen AST.Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (AST.Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (AST.Name qname)

setBlock :: AST.Name -> Codegen AST.Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen AST.Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

{-
getval :: String -> Codegen Operand
getval var = do
  s <- get
  let symvs = symval s
  case lookup var symvs of
    Just v -> return v
    Nothing -> do 
      ref <- getvar var
      val <- load ref
      modify (\s -> s { symval = (var,val):symvs})
      return val
-}

-- getval = getvar
-------------------------------------------------------------------------------

-- References
local :: AST.Name -> Operand
local = LocalReference double

idxval :: IndexSymbol -> Operand
idxval = LocalReference i64 . AST.Name


global :: AST.Name -> C.Constant
global = C.GlobalReference double

externf :: AST.Name -> Operand
externf = ConstantOperand . C.GlobalReference double

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

iadd :: Operand -> Operand -> Codegen Operand
iadd a b = instr $ AST.Add False False a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = instr $ Sub False False a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

imul :: Operand -> Operand -> Codegen Operand
imul a b = instr $ AST.Mul False False a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr $ ICmp cond a b [] 

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Array
getElementPtr :: Operand -> [Operand] -> Codegen Operand
getElementPtr arr is = instr $ GetElementPtr True arr is []



-- Control Flow
br :: AST.Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> AST.Name -> AST.Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, AST.Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

-----------------------

uncurry3 f (a,b,c) = f a b c 
  
hVar h = printf "x%x" h

mkAssign name val = assign name val >> return ()

cgen4Const name = mkAssign name . fval -- (cons (C.Float (F.Double v))) 

mkOp op h val = getvar (hVar h) >>= op val

mkAdd = mkOp fadd
mkMul = mkOp fmul

-- false = cons $ C.Float (F.Double 0.0)
-- true = cons $ C.Float (F.Double 1.0)

fval :: Double -> Operand
fval v = cons $ C.Float (F.Double v)

fzero = fval 0
fone  = fval 1

ival :: Int -> Operand
ival v = cons $ C.Int 64 (fromIntegral v)

izero = ival 0
ione = ival 1

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


  
llvmCodegen :: (?expHash :: Exp Double :->: Hash)=> String -> MExp Double -> Codegen ()
llvmCodegen name (mexpExp -> Zero)          = cgen4Const name 0
llvmCodegen name (mexpExp -> One)           = cgen4Const name 1
llvmCodegen name (mexpExp -> Delta i j)     = do
  x <- cgencond ("delta"++i++j) (icmp IP.EQ (idxval i) (idxval j)) (return fone) (return fzero)
  assign name x
  return () 
llvmCodegen name (mexpExp -> Var (Simple s))= mkAssign name (local (AST.Name s))
llvmCodegen name (mexpExp -> Var (Indexed s is)) = do
  let arr = LocalReference (ptr double) (AST.Name s)
      factors = scanr (*) 1 (tail (map (\(i,s,e) -> e-s+1)  is ) ++ [1])
  indices <- forM is $ \(i,s,_) -> do
    xref <- getvar i
    x <- load xref
    if s == 0
      then return x
      else isub x (ival s)
  (i1:irest) <- zipWithM (\x y -> if y == 1 then return x else imul x (ival y))
                  indices factors 
  theindex <- foldrM iadd i1 irest       
  ptr <- getElementPtr arr [theindex]
  val <- load ptr
  assign name val
  return ()
llvmCodegen name (mexpExp -> Val n)         = cgen4Const name n
llvmCodegen name (mexpExp -> S.Add hs)      = cgen4fold name mkAdd 0 hs 
llvmCodegen name (mexpExp -> S.Mul hs)      = cgen4fold name mkMul 1 hs 
llvmCodegen name (mexpExp -> Fun sym hs)    = return ()
llvmCodegen name (MExp (Sum is h1) m i)     = do
  sumref <- alloca double
  store sumref (fval 0)
  let innerstmt = do
        body
        s <- load sumref
        v <- getvar (hVar h_result)
        s' <- fadd s v 
        store sumref s'
        return ()
  foldr (.) id (map mkFor is) innerstmt
  rval <- load sumref
  assign name rval

  where v = justLookup h1 m
        h_result = untrie ?expHash (mexpExp v)
        (hashmap,table,depgraph) = mkDepGraphNoSum v
        bmap = HM.insert h_result v (mexpMap v)
        hs_ordered = reverse (h1 : map (\i -> table ! i) (topSort depgraph))
        es_ordered = map (flip justLookup bmap) hs_ordered
        body = mapM_ (\e -> llvmCodegen (hVar (getMHash e)) e) $ es_ordered                
        mkFor = \(i,s,e) -> cgenfor ("for_" ++ i) (i,s,e)

        
llvmAST :: (?expHash :: Exp Double :->: Hash) => String -> [Symbol] -> MExp Double -> LLVM ()
llvmAST name syms v = define double name symsllvm $ do
                        let name' = hVar h_result
                        body
                        ret =<< getvar name' 
  where mkarg (Simple v) = (double,AST.Name v)
        mkarg (Indexed v _) = (ptr double,AST.Name v)
        symsllvm = map mkarg syms 
        h_result = getMHash v
        bmap = HM.insert h_result v (mexpMap v)
        (hashmap,table,depgraph) = mkDepGraphNoSum v
        hs_ordered = reverse (h_result: map (\i -> table ! i) (topSort depgraph))
        es_ordered = map (flip justLookup bmap) hs_ordered
        body = mapM_ (\e -> llvmCodegen (hVar (getMHash e)) e) $ es_ordered        

