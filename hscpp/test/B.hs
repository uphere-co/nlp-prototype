{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module B where

import Foreign.C.Types
import Foreign.Ptr

import T
import qualified T.TH as TH

newD       = $(TH.new ''CDouble)
push_backD = $(TH.push_back ''CDouble)
printoutD  = $(TH.printout ''CDouble)
deleteD    = $(TH.delete ''CDouble)

test_double = do
  putStrLn "testing vector<double>"
  b <- newD
  mapM_ (push_backD b) [30,30.5..40]
  printoutD b
  deleteD b

  
