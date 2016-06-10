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

    case A.parseOnly pExp "((tanh((x+y))*7)+4)" of
      Left err -> print err
      Right test2 -> do
        putStr "((tanh((x+y))*7)+4) = " 
        print test2
        putStr "its differential w.r.t x = "
        TIO.putStrLn (prettyprint (diff (SmplVar X) test2))

-- test result
--
-- $ cabal build
-- $ dist/build/symbolic/symbolic
-- 
-- test = BiExp (BNTNode Add (BNTLeaf (BiExp (BNTNode Mul (BNTLeaf (Fun "tanh" (Var X))) (BNTLeaf (Val 7))))) (BNTLeaf (Val 4)))
-- prettyprint test = ((tanh(x)*7)+4)
-- ((tanh((x+y))*7)+4) = BiExp (BNTNode Add (BNTNode Mul (BNTLeaf (Fun "tanh" (BiExp (BNTNode Add (BNTLeaf (Var X)) (BNTLeaf (Var Y)))))) (BNTLeaf (Val 7))) (BNTLeaf (Val 4)))
-- its differential w.r.t x = (tanh'((x+y))*7)


    
