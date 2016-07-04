{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Print where

import           Control.Lens              (view,_1)
import           Control.Monad.Trans.State
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Data.List                 (intercalate)
import           Data.MemoTrie
import           Text.Printf
--
import           Symbolic.Type
-- 


listPrintf :: PrintfType r => String -> [String] -> r
listPrintf sep []     = printf ""
listPrintf sep (x:[]) = printf "%s" x
listPrintf sep (x:xs) = printf "%s%s%s" x sep (listPrintf sep xs :: String)

prettyPrint :: (Show a, PrintfType r) => RExp a -> r
prettyPrint RZero = printf "0"
prettyPrint ROne  = printf "1"
prettyPrint (RDelta i j) = printf "delta_%s%s" i j
prettyPrint (RVal n) = printf "%s" (show n) 
prettyPrint (RVar s) = printf "%s" (showSym s)
prettyPrint (RAdd es) = printf "(%s)" (listPrintf "+" (map prettyPrint es) :: String)
prettyPrint (RMul es) = printf "(%s)" (listPrintf "*" (map prettyPrint es) :: String)
{- prettyPrint (RFun1 s e1) = printf "(%s %s)" s (prettyPrint e1 :: String)
-- prettyPrint (RFun2 s e1 e2)
  | s == "+" || s == "*" = printf "(%s%s%s)"
                             (prettyPrint e1 :: String)
                             (s :: String)
                             (prettyPrint e2 ::String)
  | otherwise            = printf "(%s%s%s)"
                             (s :: String)
                             (prettyPrint e1 :: String)
                             (prettyPrint e2 :: String) -}
prettyPrint (RSum is e1) = printf "(sum_(%s) %s)" (showIdxSet is) (prettyPrint e1 :: String)

showIdxSet :: [(Index,Int,Int)] -> String
showIdxSet = intercalate "," . map (view _1)

dotPrint :: (Show a) => HashMap Hash (MExp a) -> Hash -> State (HashSet Hash) String
dotPrint m h = do
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

dotPrint' :: (Show a) => Hash -> Exp a -> String
dotPrint' h Zero           = printf "x%x [label=\"0\"];\n" h
dotPrint' h One            = printf "x%x [label=\"1\"];\n" h
dotPrint' h (Delta i j)    = printf "x%x [label=\"delta_%s%s\"];\n" h i j
dotPrint' h (Val n)        = printf "x%x [label=\"%s\"];\n" h (show n)
dotPrint' h (Var s)        = printf "x%x [label=\"%s\"];\n" h (showSym s)
dotPrint' h (Add hs)       = printf "x%x [label=\"+\"];\n" h ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
dotPrint' h (Mul hs)       = printf "x%x [label=\"*\"];\n" h ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
-- dotPrint' h (Fun1 s h1)    = printf "x%x [label=\"%s\"];\n%x -> x%x;\n" h s h h1
-- dotPrint' h (Fun2 s h1 h2) = printf "x%x [label=\"%s\"];\nx%x -> x%x;\nx%x -> x%x;\n" h s h h1 h h2
dotPrint' h (Sum is h1)    = printf "x%x [label=\"sum_(%s)\"];\nx%x -> x%x;\n" h (showIdxSet is) h h1

digraph :: (HasTrie a, Show a, ?expHash :: Exp a :->: Hash) => MExp a -> IO ()
digraph v = do
    let h = untrie ?expHash (mexpExp v)
        m = HM.insert h v (mexpMap v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (dotPrint m h) HS.empty
    putStrLn "}"

prettyPrintE :: (Show a, PrintfType r) => EExp a -> r
prettyPrintE (EVal n) = printf "%s" (show n)
prettyPrintE (EDelta t) = prettyPrintT t
prettyPrintE (EAdd n t) = printf "(%s+%s)" (show n) (prettyPrintT t :: String)
prettyPrintE (EAdd2 t1 t2) = printf "(%s+%s)" (prettyPrintT t1 :: String) (prettyPrintT t2 :: String)
prettyPrintE (EMul n t) = printf "(%s*%s)" (show n) (prettyPrintT t :: String)
prettyPrintE (EMul2 t1 t2) = printf "(%s*%s)" (prettyPrintT t1 :: String) (prettyPrintT t2 :: String)


prettyPrintT :: (PrintfType r) => TDelta -> r
prettyPrintT (TDelta2 i j) = printf "delta_%s%s" i j
prettyPrintT (TDelta1 i n) = printf "delta_%s%d" i n
