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
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.HashSet                    (HashSet)
import qualified Data.HashSet              as HS
import           Data.MemoTrie
import           Language.C.Data
import           Language.C.Data.Ident
import           Language.C.Data.Position
import           Language.C.Pretty
import           Language.C.Syntax
import           Text.Printf
import           Text.PrettyPrint
--
import Symbolic.Print
import Symbolic.Type

{-
cPrint :: (Show a) => HashMap Hash (MExp a) -> Hash -> State (HashSet Hash) String
cPrint m h = do
  s <- get
  let MExp e _ _ = justLookup h m
  case h `HS.member` s of
    True -> return ""
    False -> do
      let str = dotPrint' h e
          hs = daughters e
      put (h `HS.insert` s)
      lst <- mapM (dotPrint m) hs
      return (concat (str : lst))
-}


  
pos = position 0 "test" 0 0

nodeinfo = OnlyPos pos (pos,0)

ident name = Ident name 0 nodeinfo

-- CIntType
-- CVoidType

ptr = CPtrDeclr [] nodeinfo

mkIntConst v = CConst (CIntConst (cInteger (fromIntegral v)) nodeinfo)

mkDecl typ isPtr name mv =
  let cdeclr = CDeclr (Just (ident name)) (if isPtr then [ptr] else []) Nothing [] nodeinfo
      typespec = CTypeSpec (typ nodeinfo)
      mv' = fmap (\v -> CInitExpr (mkIntConst v) nodeinfo) mv
  in CDecl [typespec] [(Just cdeclr,mv',Nothing)] nodeinfo

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
 CFor (Right (mkDecl CIntType False name (Just start)))
      (Just (mkBinary name CLeqOp end))
      (Just (mkUnary name CPostIncOp))
      stmts nodeinfo

mkVar name = CVar (ident name) nodeinfo

mkUnary name op = CUnary op (mkVar name) nodeinfo
 
mkBinary name op value = CBinary op (mkVar name) (mkIntConst value) nodeinfo


cPrint' :: Hash -> Exp a -> CStat
cPrint' h Zero           = CExpr (Just (mkIntConst 0)) nodeinfo
cPrint' h One            = CExpr (Just (mkIntConst 1)) nodeinfo
cPrint' h (Sum is h1)    = foldr (.) id (map mkFor' is) (CBreak nodeinfo)
 where mkFor' (i,s,e) = mkFor i s e

  -- printf "x%x [label=\"sum_(%s)\"];\nx%x -> x%x;\n" h (showIdxSet is) h h1


{-
cPrint' h (Delta i j)    = printf "x%x [label=\"delta_%s%s\"];\n" h i j
cPrint' h (Val n)        = printf "x%x [label=\"%s\"];\n" h (show n)
cPrint' h (Var s)        = printf "x%x [label=\"%s\"];\n" h (showSym s)
cPrint' h (Add hs)       = printf "x%x [label=\"+\"];\n" h ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
cPrint' h (Mul hs)       = printf "x%x [label=\"*\"];\n" h ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
cPrint' h (Fun s hs)     = printf "x%x [label=\"%s\"];\n" h s ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
-}





       
cgraph :: (HasTrie a, Show a, ?expHash :: Exp a :->: Hash) => String -> [Symbol] -> MExp a -> IO ()
cgraph name syms v = do
  let -- decllst = [mkDecl CIntType "a"]
      bodylst = [CBlockStmt ((cPrint' 3393 (Sum [("i",1,3),("j",2,4)] 339)))] -- [CBlockDecl (mkDecl CDoubleType False "b")]  
      ctu = CTranslUnit [CFDefExt (mkCFunction CVoidType name (mkArgs syms) bodylst)] nodeinfo 
  (putStrLn . render . pretty) ctu


{-
    let h = untrie ?expHash (mexpExp v)
        m = HM.insert h v (mexpMap v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (cPrint m h) HS.empty
    putStrLn "}"
-}
