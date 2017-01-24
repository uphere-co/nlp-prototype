{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.Types
import Foreign.Ptr

import A
import B

main = do
  test_int
  test_double
  test_double2
  
