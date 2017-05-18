module Assert where

--import           Test.HUnit          (assertBool)
import Test.Tasty.HUnit (assertBool)

massertEqual :: (Eq a) => IO a -> a -> IO ()
massertEqual expr val = do
  r <- expr
  assertBool "" (r == val)
