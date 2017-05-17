module Assert where

import           Control.Monad.Primitive               (PrimMonad)

foo :: IO Bool
foo  = return True
    
assert :: (PrimMonad m) => Bool -> m ()
assert True = return ()
assert False = error "Assertion error"

assertEqual :: (PrimMonad m, Eq a) => m a -> a -> m ()
assertEqual expr val = do
  r <- expr
  assert (r == val)
