{-# LANGUAGE RecordWildCards #-}
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
--
import           Data.Array.Accelerate.Matrix


data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: Array DIM2 Float
                               , autoenc_b   :: Array DIM1 Float  
                               , autoenc_c1  :: Array DIM1 Float
                               , autoenc_c2  :: Array DIM1 Float
                               } deriving Show
                               
                           
prepare :: Vector Float -> AutoEncoder
prepare v = let arr_We  = AIO.fromVectors (Z :. 100 :. 200) ((),v)
                arr_b   = AIO.fromVectors (Z :. 200) ((),V.slice 20000 200 v)
                arr_c1  = AIO.fromVectors (Z :. 100) ((),V.slice 20200 100 v)
                arr_c2  = AIO.fromVectors (Z :. 100) ((),V.slice 20300 100 v)
            in AutoEncoder 100 arr_We arr_b arr_c1 arr_c2

findP :: AutoEncoder -> Array DIM1 Float
findP AutoEncoder {..} =
    run $ A.slice result (A.lift (Z :. All :. (0 :: Int)))
  where
        result = A.zipWith (+) (matMul (use autoenc_We) combined) blifted 
        combined = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_c1 A.++ use autoenc_c2)
        blifted  = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_b)
        

