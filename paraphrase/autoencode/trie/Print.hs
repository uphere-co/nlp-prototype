module Print where

import           Control.Monad.Trans.State
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Text.Printf
--
import           Type
-- 

prettyPrint :: PrintfType r => RExp -> r
prettyPrint RZero = printf "0"
prettyPrint ROne  = printf "1"
prettyPrint (RDelta i j) = printf "delta_%s%s" i j
prettyPrint (RVal n) = printf "%d" n 
prettyPrint (RVar s) = printf "%s" (showSym s)
prettyPrint (RFun1 s e1) = printf "(%s %s)" (showSym s) (prettyPrint e1 :: String)
prettyPrint (RFun2 s e1 e2)
  | (showSym s) == "+" || (showSym s) == "*" = printf "(%s%s%s)"
                                                 (prettyPrint e1 :: String)
                                                 (showSym s :: String)
                                                 (prettyPrint e2 ::String)
  | otherwise                                = printf "(%s%s%s)"
                                                 (showSym s :: String)
                                                 (prettyPrint e1 :: String)
                                                 (prettyPrint e2 :: String)

dotPrint :: HashMap Hash ExpMap -> Hash -> State (HashSet Hash) String
dotPrint m h = do
  s <- get
  let Just (ExpMap e _) = HM.lookup h m
  case h `HS.member` s of
    True -> return ""
    False -> do
      let (str,hs) = dotPrint' h e
      put (h `HS.insert` s)
      lst <- mapM (dotPrint m) hs
      return (concat (str : lst))

dotPrint' :: Hash -> Exp -> (String,[Hash])
dotPrint' h Zero           = (printf "x%x [label=\"0\"];\n" h,[])
dotPrint' h One            = (printf "x%x [label=\"1\"];\n" h,[])
dotPrint' h (Delta i j)    = (printf "x%x [label=\"delta_%s%s\"];\n" h i j,[])
dotPrint' h (Val n)        = (printf "x%x [label=\"%d\"];\n" h n ,[])
dotPrint' h (Var s)        = (printf "x%x [label=\"%s\"];\n" h (showSym s),[])
dotPrint' h (Fun1 s h1)    = (printf "x%x [label=\"%s\"];\n%s -> x%x;\n" h (showSym s) h h1,[h1])
dotPrint' h (Fun2 s h1 h2) = (printf "x%x [label=\"%s\"];\nx%x -> x%x;\nx%x -> x%x;\n" h (showSym s) h h1 h h2,[h1,h2])
