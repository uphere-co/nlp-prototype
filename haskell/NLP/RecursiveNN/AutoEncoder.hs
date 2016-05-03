{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NLP.RecursiveNN.AutoEncoder where

import           Data.Array.Accelerate             (use, (:.)(..), Array(..), Z(..),
                                                    DIM1, DIM2, All(..), Acc, Exp)
import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.AST  as A
import qualified Data.Array.Accelerate.Array.Sugar as S
import           Data.Array.Accelerate.CUDA
import           Data.Array.Accelerate.CUDA.Foreign
import qualified Data.Array.Accelerate.IO   as AIO
import           Data.Vector.Storable              ((!), Vector)
import qualified Data.Vector.Storable       as V
import qualified Foreign.CUDA.Driver        as CUDA
--
import           Data.Array.Accelerate.Matrix

-- | this function is defined in HEAD of accelerate, but I define it here.
-- mkTanh x = A.Exp (A.PrimTanh A.floatingType `A.PrimApp` x)


data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: Array DIM2 Float
                               , autoenc_b   :: Array DIM1 Float  
                               , autoenc_c1  :: Array DIM1 Float
                               , autoenc_c2  :: Array DIM1 Float
                               } deriving Show
                       
prepare :: Vector Float -> AutoEncoder
prepare v =
    let arr_We  = AIO.fromVectors (Z :. 100 :. 200) v
        arr_b   = AIO.fromVectors (Z :. 200) (V.slice 20000 200 v)
        arr_c1  = AIO.fromVectors (Z :. 100) (V.slice 20200 100 v)
        arr_c2  = AIO.fromVectors (Z :. 100) (V.slice 20300 100 v)
    in AutoEncoder 100 arr_We arr_b arr_c1 arr_c2

calcP :: AutoEncoder -> Vector Float
calcP AutoEncoder {..} =
    AIO.toVectors . run . A.map (tanh . (/ 100.0)) . A.slice result $ (A.lift (Z :. All :. (0 :: Int)))
  where
        result = A.zipWith (+) (matMul (use autoenc_We) combined) blifted 
        combined = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_c1 A.++ use autoenc_c2)
        blifted  = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_b)

