{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen.LLVM.Lang where

import Data.Foldable ( foldrM )
import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

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
import           Symbolic.Predefined
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
double :: Type
double = FloatingPointType 64 IEEE

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
  , symval       :: SymbolTable              -- Function scope symbol-value table
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
emptyCodegen = CodegenState (AST.Name entryBlockName) Map.empty [] [] 1 0 Map.empty

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

getval = getvar
-------------------------------------------------------------------------------

-- References
local :: AST.Name -> Operand
local = LocalReference double

global :: AST.Name -> C.Constant
global = C.GlobalReference double

externf :: AST.Name -> Operand
externf = ConstantOperand . C.GlobalReference double

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

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

mkAssign name val = do
  -- ref <- alloca double
  -- store ref val
  assign name val -- ref
  return ()

cgen4Const name v = mkAssign name (cons (C.Float (F.Double v))) 

mkOp op h val = do 
  val' <- getval (hVar h)
  -- val' <- load ref'
  op val val'

mkAdd = mkOp fadd
mkMul = mkOp fmul

cgen4fold name op ini [] = cgen4Const name ini
cgen4fold name op ini (h:hs) = do
  val1 <- getval (hVar h)
  -- ref <- alloca double
  v' <- foldrM op val1 hs
  -- store ref v'
  assign name v' -- ref
  return ()

  
llvmCodegen :: (?expHash :: Exp Double :->: Hash)=> String -> MExp Double -> Codegen ()
llvmCodegen name (mexpExp -> Zero)          = cgen4Const name 0
llvmCodegen name (mexpExp -> One)           = cgen4Const name 1
llvmCodegen name (mexpExp -> Delta i j)     = return () -- [ CIf cond  stru (Just sfal) nodeinfo ]
  -- where cond = mkBinary (mkVar i) CEqOp (mkVar j)
  --       stru = mkExpr (mkAssign name (mkConst (mkI 1)))
  --       sfal = mkExpr (mkAssign name (mkConst (mkI 0)))
llvmCodegen name (mexpExp -> Var v)         = mkAssign name (local (AST.Name rhs))
  where rhs = case v of
                Simple s -> s -- mkVar s
                -- Indexed s is -> mkIVar s is
llvmCodegen name (mexpExp -> Val n)         = cgen4Const name n
llvmCodegen name (mexpExp -> S.Add hs)      = cgen4fold name mkAdd 0 hs 
llvmCodegen name (mexpExp -> S.Mul hs)      = cgen4fold name mkMul 1 hs 
llvmCodegen name (mexpExp -> Fun sym hs)    = return ()
llvmCodegen name (MExp (Sum is h1) m i)     = return () -- [ mkExpr (mkAssign name (mkConst (mkF 0)))
                                          -- , foldr (.) id (map (uncurry3 mkFor) is) innerstmt ]
  {-
  where v = justLookup h1 m
        h_result = untrie ?expHash (mexpExp v)
        (hashmap,table,depgraph) = mkDepGraphNoSum v
        bmap = HM.insert h_result v (mexpMap v)
        hs_ordered = reverse (map (\i -> table ! i) (topSort depgraph))
        es_ordered = map (flip justLookup bmap) hs_ordered
        decllst = map (CBlockDecl . mkDblVarDecl . hVar . getMHash) es_ordered
        bodylst' = map CBlockStmt . concatMap (\e -> llvmPrint' (hVar (getMHash e)) e) $ es_ordered
        innerstmt =
          mkCompound $  
            decllst ++ bodylst' ++ [CBlockStmt (mkExpr (mkAssignAdd name (mkVar (hVar h1))))]
  -}
        
llvmAST :: (?expHash :: Exp Double :->: Hash) => String -> [Symbol] -> MExp Double -> LLVM () -- CTranslUnit
llvmAST name syms v = define double name symsllvm $ do
                        let name' = hVar h_result
                        body
                        -- ref <- getval name'
                        -- val <- load ref
                        ret =<< getval name' -- val
  where symsllvm = map ((double,) . AST.Name . varName ). filter isSimple $ syms
        h_result = getMHash v
        bmap = HM.insert h_result v (mexpMap v)
        (hashmap,table,depgraph) = mkDepGraphNoSum v
        hs_ordered = reverse (map (\i -> table ! i) (topSort depgraph))
        es_ordered = map (flip justLookup bmap) hs_ordered
        body = mapM_ (\e -> llvmCodegen (hVar (getMHash e)) e) $ es_ordered        

