{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module B where

import Foreign.C.Types
import Foreign.Ptr

import T
import qualified T.TH as TH

createD    = $(TH.create ''Double)
push_backD = $(TH.push_back ''Double)
printoutD  = $(TH.printout ''Double)


test_double = do
  putStrLn "testing vector<double>"
  b <- createD
  mapM_ (push_backD b) [30,30.5..40]
  printoutD b

  
