{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen.LLVM.Lang where

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
emptyCodegen = CodegenState (AST.Name entryBlockName) Map.empty [] 1 0 Map.empty

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
  
pos = position 0 "test" 0 0

nodeinfo = OnlyPos pos (pos,0)

ident name = Ident name 0 nodeinfo

ptr = CPtrDeclr [] nodeinfo

data Const = IConst Int
           | FConst Double

mkI = IConst
mkF = FConst

mkConst (IConst i) = CConst (CIntConst (cInteger (fromIntegral i)) nodeinfo)
mkConst (FConst f) = CConst (CFloatConst (readCFloat (show f)) nodeinfo)


mkDecl typ ptrnum name mv =
  let cdeclr = CDeclr (Just (ident name)) (replicate ptrnum ptr) Nothing [] nodeinfo
      typespec = CTypeSpec (typ nodeinfo)
      mv' = fmap (\v -> CInitExpr (mkConst v) nodeinfo) mv
  in CDecl [typespec] [(Just cdeclr,mv',Nothing)] nodeinfo

mkDblVarDecl str = mkDecl CDoubleType 0 str (Just (mkF 0))

mkCFunction typ name decllst bodylst =
  let typspec  = CTypeSpec (typ nodeinfo)
      fun = CFunDeclr (Right (decllst,False)) [] nodeinfo
      cdeclr = CDeclr (Just (ident name)) [fun] Nothing [] nodeinfo
      ccompound = CCompound [] bodylst  nodeinfo
  in CFunDef [typspec] cdeclr [] ccompound nodeinfo

mkArgs = map mkArg
 where mkArg (Simple s) = mkDecl CDoubleType 0 s Nothing
       mkArg (Indexed s is) = mkDecl CDoubleType (length is) s Nothing



mkFor name start end stmts =
 CFor (Right (mkDecl CIntType 0 name (Just (mkI start))))
      (Just (mkBinary (mkVar name) CLeqOp (mkConst (mkI end))))
      (Just (mkUnary name CPostIncOp))
      stmts nodeinfo

mkVar name = CVar (ident name) nodeinfo

mkIVar name is = foldl' (\acc i -> CIndex acc (mkVar i) nodeinfo) (mkVar name) is

mkUnary name op = CUnary op (mkVar name) nodeinfo
 
mkBinary x op y = CBinary op x y nodeinfo

mkAssign name value = CAssign CAssignOp (mkVar name) value nodeinfo

mkAssignAdd name value = CAssign CAddAssOp (mkVar name) value nodeinfo

mkExpr exp = CExpr (Just exp) nodeinfo 
mkCompound stmts = CCompound [] stmts nodeinfo 

mkCall sym lst = CCall (mkVar sym) lst nodeinfo

mkReturn exp = CReturn (Just exp) nodeinfo

hVar h = printf "x%x" h

llvmCodegen :: (?expHash :: Exp Double :->: Hash)=> String -> MExp Double -> Codegen ()
llvmCodegen name (mexpExp -> Zero)          = do
  ref <- alloca double
  -- named name ref
  store ref (cons (C.Float (F.Double 0)))
  assign name ref
  return ()
  -- assign name (cons (C.Float (F.Double 0))) -- return () -- [ mkExpr (mkAssign name (mkConst (mkI 0))) ]
llvmCodegen name (mexpExp -> One)           = undefined -- [ mkExpr (mkAssign name (mkConst (mkI 1))) ]
llvmCodegen name (mexpExp -> Delta i j)     = undefined -- [ CIf cond  stru (Just sfal) nodeinfo ]
  where cond = mkBinary (mkVar i) CEqOp (mkVar j)
        stru = mkExpr (mkAssign name (mkConst (mkI 1)))
        sfal = mkExpr (mkAssign name (mkConst (mkI 0)))
llvmCodegen name (mexpExp -> Var v)         = undefined -- [ mkExpr (mkAssign name rhs) ] 
  where rhs = case v of
                Simple s -> mkVar s
                Indexed s is -> mkIVar s is
llvmCodegen name (mexpExp -> Val n)         = undefined -- [ mkExpr (mkAssign name (mkConst (mkF n))) ]
llvmCodegen name (mexpExp -> S.Add hs)        = return () -- [ (mkExpr . mkAssign name . foldr1 (flip mkBinary CAddOp)) lst ]
  where lst = map (mkVar . hVar) hs
llvmCodegen name (mexpExp -> S.Mul hs)        = return () -- [ (mkExpr . mkAssign name . foldr1 (flip mkBinary CMulOp)) lst ]
  where lst = map (mkVar . hVar) hs
llvmCodegen name (mexpExp -> Fun sym hs)    = return () -- [ mkExpr (mkAssign name (mkCall sym lst)) ]
  where lst = map (mkVar . hVar) hs
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
llvmAST name syms v = define double name [] $ do  
                        llvmCodegen name' v
                        {- let ref = AST.Name name' -}
                        -- let ref = local (AST.Name name') 
                        --res <- load ref
                        ref <- getvar name'
                        -- ret res
                        val <- load ref
                        ret val -- (cons (C.Float (F.Double 0))) 
  where h_result = getMHash v
        name' = hVar h_result
  -- return ()
{-
  let h_result = untrie ?expHash (mexpExp v)
      (hashmap,table,depgraph) = mkDepGraphNoSum v
      bmap = HM.insert h_result v (mexpMap v)
      hs_ordered = reverse (map (\i -> table ! i) (topSort depgraph))
      es_ordered = map (flip justLookup bmap) hs_ordered
      decllst = map (CBlockDecl . mkDblVarDecl . hVar . getMHash) es_ordered
      bodylst' = map CBlockStmt . concatMap (\e -> llvmPrint' (hVar (getMHash e)) e) $ es_ordered
      bodylst = decllst ++ bodylst' ++ [CBlockStmt (mkReturn (mkVar (hVar h_result))) ]
  in return () -- CTranslUnit [CFDefExt (mkCFunction CDoubleType name (mkArgs syms) bodylst)] nodeinfo
-}

{- 
llvmPrint :: (?expHash :: Exp Double :->: Hash) => String -> [Symbol] -> MExp Double -> IO ()
llvmPrint name syms v = let ctu = llvmAST name syms v in (putStrLn . render . pretty) ctu
-}

