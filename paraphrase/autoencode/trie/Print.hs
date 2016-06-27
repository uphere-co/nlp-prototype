module Print where

import           Control.Arrow
import           Control.Lens              (over, _1)
import           Control.Monad.Trans.State
import           Data.Bits                 (xor)
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Text.Printf
--
import           Type
-- 
import Debug.Trace


prettyPrint RZero = "0"
prettyPrint ROne  = "1"
prettyPrint (RVal n) = show n 
prettyPrint (RVar s) = s
prettyPrint (RFun1 s e1) = printf "( %s %s )" s (prettyPrint e1)
prettyPrint (RFun2 s e1 e2) = printf "( %s %s %s )" s (prettyPrint e1) (prettyPrint e2)

dotPrint :: HashMap Int Exp -> Hash -> State (HashSet Int) String
dotPrint m h = do
  s <- get
  let Just e = HM.lookup h m
  case h `HS.member` s of
    True -> return ""
    False -> do
      let (str,hs) = dotPrint' h e
      put (h `HS.insert` s)
      lst <- mapM (dotPrint m) hs
      return (concat (str : lst))
      -- in str ++ concatMap dotPrint m (hs `HS.union` s) 

dotPrint' h Zero           = (printf "x%x [label=\"0\"];\n" h,[])
dotPrint' h One            = (printf "x%x [label=\"1\"];\n" h,[])
dotPrint' h (Val n)        = (printf "x%x [label=\"%d\"];\n" h n ,[])
dotPrint' h (Var s)        = (printf "x%x [label=\"%s\"];\n" h s,[])
dotPrint' h (Fun1 s h1)    = (printf "x%x [label=\"%s\"];\n%s -> x%x;\n" h s h h1,[h1]) --  ++ dotPrint ms h1
dotPrint' h (Fun2 s h1 h2) = (printf "x%x [label=\"%s\"];\nx%x -> x%x;\nx%x -> x%x;\n" h s h h1 h h2,[h1,h2])
