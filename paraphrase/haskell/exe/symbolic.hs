{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as TIO
--
import           NLP.SyntaxTree.Type
import           Symbolic.Type
import           Symbolic.Parser

test :: Exp
test = BiExp $ BNTNode Add (BNTNode Mul (BNTLeaf (Fun Tanh (Var X))) (BNTLeaf (Val 7))) (BNTLeaf (Val 4))


main :: IO ()
main = do
    putStrLn $ "test = " ++ show test
    putStr $ "prettyprint test = "
    TIO.putStrLn (prettyprint test)
  
    print (A.parseOnly pExp "((tanh((x+y))*7)+4)")

-- test result
--
-- $ cabal build
-- $ dist/build/symbolic/symbolic
-- 
-- test = BiExp (BNTNode Add (BNTNode Mul (BNTLeaf (Fun Tanh (Var X))) (BNTLeaf (Val 7))) (BNTLeaf (Val 4)))
-- prettyprint test = ((tanh(x)*7)+4)
-- Right (BiExp (BNTNode Add (BNTNode Mul (BNTLeaf (Fun Tanh (BiExp (BNTNode Add (BNTLeaf (Var X)) (BNTLeaf (Var Y)))))) (BNTLeaf (Val 7))) (BNTLeaf (Val 4))))



    
