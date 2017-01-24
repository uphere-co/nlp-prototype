{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module A where

import Control.Exception
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import STL
import qualified STL.TH as TH
import STL.Instances 

withVec :: (ISTLVector a) => (STLVector a -> IO ()) -> IO ()
withVec = bracket new delete 


test_int = do 
  putStrLn "testing vector<int>"
  withVec $ \(a :: STLVector CInt) -> do
    mapM_ (push_back a) [10,20..100]
    printout a


test_double2 = do
  putStrLn "test_double2"
  withVec $ \(b :: STLVector CDouble) -> do
    mapM_ (push_back b) [1.1,1.2..2.0]
    printout b
    c <- at b 5
    v <- peek c
    putStrLn ("b[5]= " ++ show v)
