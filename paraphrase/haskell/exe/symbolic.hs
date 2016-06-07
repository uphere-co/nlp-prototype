{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import           Data.Text (Text (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
--
import           NLP.SyntaxTree.Type

data Operation = Add | Mul deriving (Show, Eq)

-- data Exp = Op Operation
--          | Val Int

test :: BNTree Operation Int
test = BNTNode Add (BNTNode Mul (BNTLeaf 3) (BNTLeaf 7)) (BNTLeaf 4)

showOp Add = "+"
showOp Mul = "*"

prettyprint :: BNTree Operation Int -> Text
prettyprint (BNTNode o x y) = "(" <> prettyprint x <> showOp o <> prettyprint y <> ")"
prettyprint (BNTLeaf x) = T.pack (show x)


main :: IO ()
main = do
    putStrLn "symbolic calculation test"
    print test
    TIO.putStrLn (prettyprint test)
  
