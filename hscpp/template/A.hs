{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module A where

import Foreign.C.Types
import Foreign.Ptr

import T
import qualified T.TH as TH

create    = $(TH.create ''Int)
push_back = $(TH.push_back ''Int)
printout  = $(TH.printout ''Int)

createD    = $(TH.create ''Double)
push_backD = $(TH.push_back ''Double)
printoutD  = $(TH.printout ''Double)

test_int = do 
  putStrLn "testing vector<int>"
  a <- create
  mapM_ (push_back a) [10,20..100]
  printout a


test_double2 = do
  b <- createD
  mapM_ (push_backD b) [1.1,1.2..2.0]
  printoutD b

