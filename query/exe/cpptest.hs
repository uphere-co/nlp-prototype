{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Foreign.C.Types
import Foreign.Ptr

import           Query.Binding.Vector.Template
import qualified Query.Binding.Vector.TH as TH

 
import           Query.Binding.Engine


$(TH.genVectorInstanceFor ''CInt "int")
---    $(TH.genVectorInstanceFor ''Engine  "Engine")

main = do
  v :: Vector CInt <- newVector
  n <- size v 
  print =<< size v

  push_back v 1
  print =<< size v
  mapM_ (push_back v) [1..100]
  print =<< size v
  pop_back v
  print =<< size v

  print =<< at v 5
  deleteVector v


  --
  f <- newEngine 9
  showme f
  v2 <- getVector f
  print =<< size v2
  print =<< v2 `at` 0
  print =<< v2 `at` 2

  v3 <- newVector
  mapM_ (push_back v3) [1..1000]

  addContents f v3
  print =<< size v2

  
{- 
  g <- newFoo 10
  w <- newVector
  push_back w g

  -- pop_back w

  print =<< size w

  x <- at w 0
  showme x
  deleteVector w
  
  --


-}
