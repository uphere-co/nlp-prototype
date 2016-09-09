{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module STL.Instances where

import Foreign.C.Types
import Foreign.Ptr

import  STL
import qualified STL.TH as TH

instance ISTLVector CInt where
  new       = $(TH.new       ''CInt)
  push_back = $(TH.push_back ''CInt)
  printout  = $(TH.printout  ''CInt)
  at        = $(TH.at        ''CInt)
  delete    = $(TH.delete    ''CInt)

instance ISTLVector CDouble where
  new       = $(TH.new       ''CDouble)
  push_back = $(TH.push_back ''CDouble)
  printout  = $(TH.printout  ''CDouble)
  at        = $(TH.at        ''CDouble)
  delete    = $(TH.delete    ''CDouble)


