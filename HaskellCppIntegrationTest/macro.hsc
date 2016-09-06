{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr

data HelloWorld a

foreign import ccall "helloworld" c_helloworld :: Ptr (HelloWorld a) -> IO ()

main = do
  putStrLn "Hello world"
  c_helloworld nullPtr
  -- c_lib_link_test
  
