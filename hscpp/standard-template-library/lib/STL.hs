{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STL where

import Foreign.C.Types
import Foreign.Ptr

data STLVector a

class ISTLVector a where
  printout  :: Ptr (STLVector a) -> IO ()
  new       :: IO (Ptr (STLVector a))
  push_back :: Ptr (STLVector a) -> a -> IO ()
  at        :: Ptr (STLVector a) -> CInt -> IO (Ptr a)
  delete    :: Ptr (STLVector a) -> IO ()

