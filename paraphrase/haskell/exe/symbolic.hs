{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Monoid
import           Data.Text (Text (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
--
import           NLP.SyntaxTree.Type

data Operation = Add | Mul deriving (Show, Eq)

test :: BNTree Operation Int
test = BNTNode Add (BNTNode Mul (BNTLeaf 3) (BNTLeaf 7)) (BNTLeaf 4)

showOp Add = "+"
showOp Mul = "*"

prettyprint :: BNTree Operation Int -> Text
prettyprint (BNTNode o x y) = "(" <> prettyprint x <> showOp o <> prettyprint y <> ")"
prettyprint (BNTLeaf x) = T.pack (show x)

pMath :: A.Parser (BNTree Operation Int)
pMath = pNode 

pInt = BNTLeaf <$> A.decimal

pOp = (A.char '+' >> return Add) <|> (A.char '*' >> return Mul)

pNode = do
    A.char '('
    x <- (pNode <|> pInt)
    o <- pOp
    y <- (pNode <|> pInt)
    A.char ')'
    return (BNTNode o x y)


main :: IO ()
main = do
    putStrLn "symbolic calculation test"
    print test
    TIO.putStrLn (prettyprint test)
  
    print (A.parseOnly pMath "((3*7)+4)")

-- test result
--
-- $ cabal build
-- $ dist/build/symbolic/symbolic
-- 
-- BNTNode Add (BNTNode Mul (BNTLeaf 3) (BNTLeaf 7)) (BNTLeaf 4)
-- ((3*7)+4)
-- Right (BNTNode Add (BNTNode Mul (BNTLeaf 3) (BNTLeaf 7)) (BNTLeaf 4))


    
