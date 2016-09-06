{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module B where

import Foreign.C.Types
import Foreign.Ptr

import T


-- foreign import ccall "myfuncwrapper" c_myfuncwrapper :: CInt -> IO CInt

test_double = do
  putStrLn "testing vector<double>"
  -- c_helloworld nullPtr
  -- c_lib_link_test
  c_myfuncwrapper 9
  
