{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module AModule where

import Foreign.C.Types
import Foreign.Ptr

data STLVector a
{-
foreign import ccall "helloworld_int"    c_helloworld_int
  :: Ptr (STLVector Int)   -> IO ()

foreign import ccall "helloworld_double" c_helloworld_double
  :: Ptr (STLVector Double) -> IO ()

foreign import ccall "helloworld_string" c_helloworld_string
  :: Ptr (STLVector String) -> IO ()

class HelloWorld a where
  helloworld :: STLVector a -> IO ()

instance HelloWorld Int where
  helloworld = c_helloworld_int

instance HelloWorld Double where
  helloworld = c_helloworld_double

instance HelloWorld String where
  helloworld = c_helloworld_string
-}

foreign import ccall "myfuncwrapper" c_myfuncwrapper :: CInt -> IO CInt


mymain = do
  putStrLn "Hello world"
  -- c_helloworld nullPtr
  -- c_lib_link_test
  c_myfuncwrapper 9
  
