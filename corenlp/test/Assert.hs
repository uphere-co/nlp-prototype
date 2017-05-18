module Assert where

import Test.Tasty.HUnit                (Assertion, assertBool,assertEqual)

massertEqual :: (Eq a) => IO a -> a -> IO ()
massertEqual expr val = do
  r <- expr
  assertBool "" (r == val)

eassertEqual :: (Eq a, Show a) => a -> a -> Assertion
eassertEqual = assertEqual ""
