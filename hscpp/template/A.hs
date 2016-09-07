{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module A where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import T
import qualified T.TH as TH

create    = $(TH.create ''CInt)
push_back = $(TH.push_back ''CInt)
printout  = $(TH.printout ''CInt)
delete    = $(TH.delete ''CInt)

createD    = $(TH.create ''CDouble)
push_backD = $(TH.push_back ''CDouble)
printoutD  = $(TH.printout ''CDouble)
atD        = $(TH.at ''CDouble)
deleteD    = $(TH.delete ''CDouble)

-- create2 = $(TH.create ''CInt)

withVecI :: (Ptr (STLVector CInt) -> IO ()) -> IO ()
withVecI f = do v <- create
                f v
                delete v


test_int = do 
  putStrLn "testing vector<int>"
  -- a <- create
  withVecI $ \a -> do
    mapM_ (push_back a) [10,20..100]
    printout a


test_double2 = do
  b <- createD
  mapM_ (push_backD b) [1.1,1.2..2.0]
  printoutD b

  c <- atD b 5
  v <- peek c
  print  v
