{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module A where

import Control.Exception
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import  STL
import qualified STL.TH as TH

new       = $(TH.new ''CInt)
push_back = $(TH.push_back ''CInt)
printout  = $(TH.printout ''CInt)
delete    = $(TH.delete ''CInt)

newD       = $(TH.new ''CDouble)
push_backD = $(TH.push_back ''CDouble)
printoutD  = $(TH.printout ''CDouble)
atD        = $(TH.at ''CDouble)
deleteD    = $(TH.delete ''CDouble)

withVecI :: (Ptr (STLVector CInt) -> IO ()) -> IO ()
withVecI = bracket new delete 

withVecD :: (Ptr (STLVector CDouble) -> IO ()) -> IO ()
withVecD = bracket newD deleteD

test_int = do 
  putStrLn "testing vector<int>"
  withVecI $ \a -> do
    mapM_ (push_back a) [10,20..100]
    printout a


test_double2 = do
  putStrLn "test_double2"
  withVecD $ \b -> do
    mapM_ (push_backD b) [1.1,1.2..2.0]
    printoutD b
    c <- atD b 5
    v <- peek c
    putStrLn ("b[5]= " ++ show v)
