{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NLP.RecursiveNN.AutoEncoder where

import           Data.Array.Accelerate             (use, (:.)(..), Array(..), Z(..),
                                                    DIM1, DIM2, All(..), Acc, Exp)
import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.Array.Sugar as S
import           Data.Array.Accelerate.CUDA
import           Data.Array.Accelerate.CUDA.Foreign
import qualified Data.Array.Accelerate.IO   as AIO
import           Data.Vector.Storable              ((!), Vector)
import qualified Data.Vector.Storable       as V
import qualified Foreign.CUDA.Driver        as CUDA
--
import           Data.Array.Accelerate.Matrix


data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: Array DIM2 Float
                               , autoenc_b   :: Array DIM1 Float  
                               , autoenc_c1  :: Array DIM1 Float
                               , autoenc_c2  :: Array DIM1 Float
                               } deriving Show
                       
prepare :: Vector Float -> AutoEncoder
prepare v =
    let arr_We  = AIO.fromVectors (Z :. 100 :. 200) ((),v)
        arr_b   = AIO.fromVectors (Z :. 200) ((),V.slice 20000 200 v)
        arr_c1  = AIO.fromVectors (Z :. 100) ((),V.slice 20200 100 v)
        arr_c2  = AIO.fromVectors (Z :. 100) ((),V.slice 20300 100 v)
    in AutoEncoder 100 arr_We arr_b arr_c1 arr_c2

calcP :: AutoEncoder -> Vector Float
calcP AutoEncoder {..} =
    V.map tanh . snd . AIO.toVectors $ run $ A.slice result (A.lift (Z :. All :. (0 :: Int)))
  where
        result = A.zipWith (+) (matMul (use autoenc_We) combined) blifted 
        combined = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_c1 A.++ use autoenc_c2)
        blifted  = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_b)

{- 

cudaTanh :: Acc (Array DIM1 Float) -> Acc (Array DIM1 Float)
cudaTanh = undefined
  where foreignTanh :: Array DIM1 Float -> CIO (Array DIM1 Float)
        foreignTanh arr = do
          output <- allocateArray (S.shape arr)
          iptr   <- floatPtr arr
          optr   <- floatPtr output
          liftIO $ execute iptr optr
          return output

        execute :: CUDA.DevicePtr Float -> CUDA.DevicePtr Float -> IO ()
        execute iptr optr =  execC2C undefined iptr optr undefined


        floatPtr = undefined
-}
