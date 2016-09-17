{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.String

foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query"          c_query          :: CString -> IO ()
foreign import ccall "query_finalize" c_query_finalize :: IO ()


main = do
  withCString "config.json" $ \configfile -> do
    withCString "/data/groups/uphere/similarity_test/input.json" $ \queryfile -> do
      c_query_init configfile
      c_query queryfile
      c_query_finalize


