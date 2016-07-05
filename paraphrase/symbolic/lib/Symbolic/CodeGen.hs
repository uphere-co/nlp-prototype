{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen where

import           Control.Lens                    (view, _1)
import           Control.Monad.Trans.State
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
import           Text.PrettyPrint
--
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--
import           Debug.Trace


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


mkDecl typ isPtr name mv =
  let cdeclr = CDeclr (Just (ident name)) (if isPtr then [ptr] else []) Nothing [] nodeinfo
      typespec = CTypeSpec (typ nodeinfo)
      mv' = fmap (\v -> CInitExpr (mkConst v) nodeinfo) mv
  in CDecl [typespec] [(Just cdeclr,mv',Nothing)] nodeinfo

mkDblVarDecl str = mkDecl CDoubleType False str (Just (mkF 0))

mkCFunction typ name decllst bodylst =
  let typspec  = CTypeSpec (typ nodeinfo)
      fun = CFunDeclr (Right (decllst,False)) [] nodeinfo
      cdeclr = CDeclr (Just (ident name)) [fun] Nothing [] nodeinfo
      ccompound = CCompound [] bodylst  nodeinfo
  in CFunDef [typspec] cdeclr [] ccompound nodeinfo

mkArgs = map mkArg
 where mkArg (Simple s) = mkDecl CDoubleType False s Nothing
       mkArg (Indexed s is) = mkDecl CDoubleType True s Nothing

mkFor name start end stmts =
 CFor (Right (mkDecl CIntType False name (Just (mkI start))))
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

cPrint' :: (?expHash :: Exp Double :->: Hash)=> String -> MExp Double -> [CStat] 
cPrint' name (mexpExp -> Zero)          = [ mkExpr (mkAssign name (mkConst (mkI 0))) ]
cPrint' name (mexpExp -> One)           = [ mkExpr (mkAssign name (mkConst (mkI 1))) ]
cPrint' name (mexpExp -> Delta i j)     = [ CIf cond  stru (Just sfal) nodeinfo ]
  where cond = mkBinary (mkVar i) CEqOp (mkVar j)
        stru = mkExpr (mkAssign name (mkConst (mkI 1)))
        sfal = mkExpr (mkAssign name (mkConst (mkI 0)))
cPrint' name (mexpExp -> Var v)         = [ mkExpr (mkAssign name rhs) ] 
  where rhs = case v of
                Simple s -> mkVar s
                Indexed s is -> mkIVar s is
cPrint' name (mexpExp -> Val n)         = [ mkExpr (mkAssign name (mkConst (mkF n))) ]
cPrint' name (mexpExp -> Add hs)        = [ (mkExpr . mkAssign name . foldr1 (flip mkBinary CAddOp)) lst ]
  where lst = map (mkVar . hVar) hs
cPrint' name (mexpExp -> Mul hs)        = [ (mkExpr . mkAssign name . foldr1 (flip mkBinary CMulOp)) lst ]
  where lst = map (mkVar . hVar) hs
cPrint' name (mexpExp -> Fun sym hs)    = [ mkExpr (mkAssign name (mkCall sym lst)) ]
  where lst = map (mkVar . hVar) hs
cPrint' name (MExp (Sum is h1) m i)     = [ mkExpr (mkAssign name (mkConst (mkF 0)))
                                          , foldr (.) id (map (uncurry3 mkFor) is) innerstmt ]
  where v = justLookup h1 m
        h_result = untrie ?expHash (mexpExp v)
        (hashmap,table,depgraph) = mkDepGraphNoSum v
        bmap = HM.insert h_result v (mexpMap v)
        hs_ordered = reverse (map (\i -> table ! i) (topSort depgraph))
        es_ordered = map (flip justLookup bmap) hs_ordered
        decllst = map (CBlockDecl . mkDblVarDecl . hVar . getMHash) es_ordered
        bodylst' = map CBlockStmt . concatMap (\e -> cPrint' (hVar (getMHash e)) e) $ es_ordered
        innerstmt =
          mkCompound $  
            decllst ++ bodylst' ++ [CBlockStmt (mkExpr (mkAssignAdd name (mkVar (hVar h1))))]

        
cAST :: (?expHash :: Exp Double :->: Hash) => String -> [Symbol] -> MExp Double -> CTranslUnit
cAST name syms v = 
  let h_result = untrie ?expHash (mexpExp v)
      (hashmap,table,depgraph) = mkDepGraphNoSum v
      bmap = HM.insert h_result v (mexpMap v)
      hs_ordered = reverse (map (\i -> table ! i) (topSort depgraph))
      es_ordered = map (flip justLookup bmap) hs_ordered
      decllst = map (CBlockDecl . mkDblVarDecl . hVar . getMHash) es_ordered
      bodylst' = map CBlockStmt . concatMap (\e -> cPrint' (hVar (getMHash e)) e) $ es_ordered
      bodylst = decllst ++ bodylst' ++ [CBlockStmt (mkReturn (mkVar (hVar h_result))) ]
  in CTranslUnit [CFDefExt (mkCFunction CDoubleType name (mkArgs syms) bodylst)] nodeinfo

cPrint :: (?expHash :: Exp Double :->: Hash) => String -> [Symbol] -> MExp Double -> IO ()
cPrint name syms v = let ctu = cAST name syms v in (putStrLn . render . pretty) ctu


