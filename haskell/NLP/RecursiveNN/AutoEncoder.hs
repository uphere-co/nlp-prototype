{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NLP.RecursiveNN.AutoEncoder where

import           Data.Array.Accelerate             (use, (:.)(..), Array(..), Z(..),
                                                    DIM1, DIM2, All(..), Acc, Exp)
import qualified Data.Array.Accelerate      as A
import           Data.Array.Accelerate.CUDA
import qualified Data.Array.Accelerate.IO   as AIO
import           Data.Vector.Storable              ((!), Vector)
import qualified Data.Vector.Storable       as V


prepare :: 
