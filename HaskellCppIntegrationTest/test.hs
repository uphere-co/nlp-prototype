{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.Types
import Foreign.Ptr

data HelloWorld a

-- foreign import ccall "helloworld" c_helloworld :: Ptr (HelloWorld a) -> IO ()

foreign import ccall "myfuncwrapper" c_myfuncwrapper :: CInt -> IO CInt


main = do
  putStrLn "Hello world"
  -- c_helloworld nullPtr
  -- c_lib_link_test
  c_myfuncwrapper 9
  
