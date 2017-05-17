module Assert where

import           Control.Monad.Primitive               (PrimMonad)
    
assert :: Bool -> IO ()
assert True = return ()
assert False = error "Assertion error"

--assertEqual :: (PrimMonad m) => m (a) -> a -> IO()
assertEqual expr val = do
  r <- expr
  assert (r == val)
