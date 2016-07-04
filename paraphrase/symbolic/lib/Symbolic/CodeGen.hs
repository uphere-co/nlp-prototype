{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen where

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

cPrint' :: (Show a) => Hash -> Exp a -> String
cPrint' h Zero           = printf "x%x [label=\"0\"];\n" h
cPrint' h One            = printf "x%x [label=\"1\"];\n" h
cPrint' h (Delta i j)    = printf "x%x [label=\"delta_%s%s\"];\n" h i j
cPrint' h (Val n)        = printf "x%x [label=\"%s\"];\n" h (show n)
cPrint' h (Var s)        = printf "x%x [label=\"%s\"];\n" h (showSym s)
cPrint' h (Add hs)       = printf "x%x [label=\"+\"];\n" h ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
cPrint' h (Mul hs)       = printf "x%x [label=\"*\"];\n" h ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
cPrint' h (Fun s hs)     = printf "x%x [label=\"%s\"];\n" h s ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
cPrint' h (Sum is h1)    = printf "x%x [label=\"sum_(%s)\"];\nx%x -> x%x;\n" h (showIdxSet is) h h1

cgraph :: (HasTrie a, Show a, ?expHash :: Exp a :->: Hash) => MExp a -> IO ()
cgraph v = do
  -- cstr <- newCString "hello"
  let pos = position 0 "test" 0 0
      nodeinfo = OnlyPos pos (pos,0)
      ident = Ident "test" 0 nodeinfo
      typespec = CTypeSpec (CIntType nodeinfo)
      void = CTypeSpec (CVoidType nodeinfo)
      ptr = CPtrDeclr [] nodeinfo
      fun = CFunDeclr (Right ([],False)) [] nodeinfo
      -- attr = CAttr ident [] nodeinfo
      cdeclr = CDeclr (Just ident) [fun] Nothing [] nodeinfo
      cconst = CConst (CIntConst (cInteger 1) nodeinfo)
      cinit = CInitExpr cconst nodeinfo
      cdecl = CDecl [typespec] [(Just cdeclr,Just cinit,Nothing)] nodeinfo
      ccompound = CCompound [] [] nodeinfo
      
      cfundef = CFunDef [void] cdeclr [cdecl,cdecl] ccompound nodeinfo
      ctu = CTranslUnit [CDeclExt cdecl,CFDefExt cfundef] nodeinfo 
  (putStrLn . render . pretty) ctu -- fundef
{-
    let h = untrie ?expHash (mexpExp v)
        m = HM.insert h v (mexpMap v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (cPrint m h) HS.empty
    putStrLn "}"
-}
