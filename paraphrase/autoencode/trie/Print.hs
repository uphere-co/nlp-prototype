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

prettyPrint :: RExp -> String
prettyPrint RZero = "0"
prettyPrint ROne  = "1"
prettyPrint (RVal n) = show n 
prettyPrint (RVar s) = (showSym s)
prettyPrint (RFun1 s e1) = printf "( %s %s )" (showSym s) (prettyPrint e1)
prettyPrint (RFun2 s e1 e2)
  | (showSym s) == "+" || (showSym s) == "*" = printf "( %s %s %s )" (prettyPrint e1) (showSym s) (prettyPrint e2)
  | otherwise            = printf "( %s %s %s )" (showSym s) (prettyPrint e1) (prettyPrint e2)

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
dotPrint' h (Val n)        = (printf "x%x [label=\"%d\"];\n" h n ,[])
dotPrint' h (Var s)        = (printf "x%x [label=\"%s\"];\n" h (showSym s),[])
dotPrint' h (Fun1 s h1)    = (printf "x%x [label=\"%s\"];\n%s -> x%x;\n" h (showSym s) h h1,[h1])
dotPrint' h (Fun2 s h1 h2) = (printf "x%x [label=\"%s\"];\nx%x -> x%x;\nx%x -> x%x;\n" h (showSym s) h h1 h h2,[h1,h2])
