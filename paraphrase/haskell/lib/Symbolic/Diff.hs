{-# LANGUAGE OverloadedStrings #-}

module Symbolic.Diff where

import Data.Monoid
--
import NLP.SyntaxTree.Type
import Symbolic.Type

suffix' = (<> "'")

diff :: Variable -> Exp -> Exp
diff s (Fun f e)            = Fun (suffix' f) e /*/ diff s e -- chain rule
diff s (Var s') | s==s'     = One
                | otherwise = Zero
diff s (Val _)              = Zero
diff s (BiExp biexp)        = diff_bi s biexp


diff_bi :: Variable -> BNTree BiOp Exp -> Exp
diff_bi s (BNTNode Add e1 e2) = (diff_bi s e1) /+/ (diff_bi s e2)
diff_bi s (BNTNode Mul e1 e2) = ((diff_bi s e1) /*/ lift e2) /+/ (lift e1 /*/ (diff_bi s e2)) -- Leibniz rule
diff_bi s (BNTLeaf e) = diff s e 


