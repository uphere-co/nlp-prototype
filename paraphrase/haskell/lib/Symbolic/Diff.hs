{-# LANGUAGE OverloadedStrings #-}

module Symbolic.Diff where

import Data.Monoid
--
import NLP.SyntaxTree.Type
import Symbolic.Type

suffix' = (<> "'")

diff :: Variable -> Exp -> Exp
diff s (Fun f e)            = Fun (suffix' f) e /*/ diff s e -- chain rule
diff s (Var s') = diffVar s s'
diff s (Val _)              = Zero
diff s (BiExp biexp)        = diff_bi s biexp


diffVar (SmplVar s)  (SmplVar s')   | s == s'   = One
                                    | otherwise = Zero
diffVar (IdxVar s _) (SmplVar s')               = Zero
diffVar (SmplVar s)  (IdxVar s' _)              = Zero
diffVar (IdxVar s i) (IdxVar s' i') | s == s'   = Delta i' i
                                    | otherwise = Zero

 -- (SmplVar s) (SmplVar s') | s==s'     = One
     --                          | otherwise = Zero

diff_bi :: Variable -> BNTree BiOp Exp -> Exp
diff_bi s (BNTNode Add e1 e2) = (diff_bi s e1) /+/ (diff_bi s e2)
diff_bi s (BNTNode Mul e1 e2) = ((diff_bi s e1) /*/ lift e2) /+/ (lift e1 /*/ (diff_bi s e2)) -- Leibniz rule
diff_bi s (BNTLeaf e) = diff s e 


