{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module A where

import Foreign.C.Types
import Foreign.Ptr

import T
import T.TH

-- foreign import ccall "myfuncwrapper" c_myfuncwrapper :: CInt -> IO CInt

test_int = do 
  putStrLn "testing vector<int>"
  -- c_helloworld nullPtr
  -- c_lib_link_test
  -- c_myfuncwrapper 9

  $(testfunction ''Int) undefined
