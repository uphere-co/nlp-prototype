{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Print where

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

prettyPrint :: PrintfType r => RExp -> r
prettyPrint RZero = printf "0"
prettyPrint ROne  = printf "1"
prettyPrint (RDelta i j) = printf "delta_%s%s" i j
prettyPrint (RVal n) = printf "%d" n 
prettyPrint (RVar s) = printf "%s" (showSym s)
prettyPrint (RFun1 s e1) = printf "(%s %s)" s (prettyPrint e1 :: String)
prettyPrint (RFun2 s e1 e2)
  | s == "+" || s == "*" = printf "(%s%s%s)"
                             (prettyPrint e1 :: String)
                             (s :: String)
                             (prettyPrint e2 ::String)
  | otherwise            = printf "(%s%s%s)"
                             (s :: String)
                             (prettyPrint e1 :: String)
                             (prettyPrint e2 :: String)
prettyPrint (RSum is e1) = printf "(sum_(%s) %s)" (showIdxSet is) (prettyPrint e1 :: String)

showIdxSet :: [Index] -> String
showIdxSet = intercalate ","

dotPrint :: HashMap Hash MExp -> Hash -> State (HashSet Hash) String
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

dotPrint' :: Hash -> Exp -> String
dotPrint' h Zero           = printf "x%x [label=\"0\"];\n" h
dotPrint' h One            = printf "x%x [label=\"1\"];\n" h
dotPrint' h (Delta i j)    = printf "x%x [label=\"delta_%s%s\"];\n" h i j
dotPrint' h (Val n)        = printf "x%x [label=\"%d\"];\n" h n
dotPrint' h (Var s)        = printf "x%x [label=\"%s\"];\n" h (showSym s)
dotPrint' h (Fun1 s h1)    = printf "x%x [label=\"%s\"];\n%s -> x%x;\n" h s h h1
dotPrint' h (Fun2 s h1 h2) = printf "x%x [label=\"%s\"];\nx%x -> x%x;\nx%x -> x%x;\n" h s h h1 h h2
dotPrint' h (Sum is h1)    = printf "x%x [label=\"sum_(%s)\"];\nx%x -> x%x;\n" h (showIdxSet is) h h1

digraph :: (?expHash :: Exp :->: Hash) => MExp -> IO ()
digraph v = do
    let h = untrie ?expHash (mexpExp v)
        m = HM.insert h v (mexpMap v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (dotPrint m h) HS.empty
    putStrLn "}"
