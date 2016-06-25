{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as TIO
--
import           NLP.SyntaxTree.Type
import           Symbolic.Diff
import           Symbolic.Type
import           Symbolic.Parser

test :: Exp
test = (Fun "tanh" (Var (SmplVar X)) /*/ (Val 7)) /+/ Val 4


main :: IO ()
main = do
    putStrLn $ "test = " ++ show test
    putStr $ "prettyprint test = "
    TIO.putStrLn (prettyprint test)
    putStrLn "-----------------"
    case A.parseOnly pExp "((tanh((x+y))*7)+4)" of
      Left err -> print err
      Right test2 -> do
        putStr "((tanh((x+y))*7)+4) = " 
        print test2
        putStr "its differential w.r.t x = "
        TIO.putStrLn (prettyprint (diff (SmplVar X) test2))
    putStrLn "-----------------"
    case A.parseOnly pExp "((tanh((x_i1+y_i1))*3)+4)" of
      Left err -> print err
      Right test3 -> do
        putStr "((tanh((x_i1+y_i1))*3)+4) = "
        print test3
        putStr "its differential w.r.t x_i2 = "
        TIO.putStrLn (prettyprint (diff (IdxVar X (Idx 2)) test3))
    putStrLn "-----------------"        
    case A.parseOnly pExp "((x_i1+y_i1)*(x_i1+y_i1))" of
      Left err -> print err
      Right test3 -> do
        putStr "((x_i1-y_i1)*(x_i1-y_i1)) = "
        print test3
        putStr "its differential w.r.t x_i2 = "
        TIO.putStrLn (prettyprint (diff (IdxVar X (Idx 2)) test3))

              
-- test result
--
-- $ cabal build
-- $ dist/build/symbolic/symbolic
-- 
-- test = BiExp (BNTNode Add (BNTLeaf (BiExp (BNTNode Mul (BNTLeaf (Fun "tanh" (Var (SmplVar X)))) (BNTLeaf (Val 7))))) (BNTLeaf (Val 4)))
-- prettyprint test = ((tanh(x)*7)+4)
-- -----------------
-- ((tanh((x+y))*7)+4) = BiExp (BNTNode Add (BNTNode Mul (BNTLeaf (Fun "tanh" (BiExp (BNTNode Add (BNTLeaf (Var (SmplVar X))) (BNTLeaf (Var (SmplVar Y))))))) (BNTLeaf (Val 7))) (BNTLeaf (Val 4)))
-- its differential w.r.t x = (tanh'((x+y))*7)
-- -----------------
-- ((tanh((x_i1+y_i1))*3)+4) = BiExp (BNTNode Add (BNTNode Mul (BNTLeaf (Fun "tanh" (BiExp (BNTNode Add (BNTLeaf (Var (IdxVar X (Idx 1)))) (BNTLeaf (Var (IdxVar Y (Idx 1)))))))) (BNTLeaf (Val 3))) (BNTLeaf (Val 4)))
-- its differential w.r.t x_i2 = (tanh'((x_i2+y_i2))*3)
-- -----------------
-- ((x_i1-y_i1)*(x_i1-y_i1)) = BiExp (BNTNode Mul (BNTNode Add (BNTLeaf (Var (IdxVar X (Idx 1)))) (BNTLeaf (Var (IdxVar Y (Idx 1))))) (BNTNode Add (BNTLeaf (Var (IdxVar X (Idx 1)))) (BNTLeaf (Var (IdxVar Y (Idx 1))))))
-- its differential w.r.t x_i2 = ((x_i2+y_i2)+(x_i2+y_i2))
