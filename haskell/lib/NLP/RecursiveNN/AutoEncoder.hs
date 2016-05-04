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
import           Data.Vector.Storable.Matrix

-- | this function is defined in HEAD of accelerate, but I define it here.
-- mkTanh x = A.Exp (A.PrimTanh A.floatingType `A.PrimApp` x)


data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: Array DIM2 Float
                               , autoenc_b   :: Array DIM1 Float  
                               , autoenc_c1  :: Array DIM1 Float
                               , autoenc_c2  :: Array DIM1 Float
                               } deriving Show

data AutoEncoder' = AutoEncoder' { autoenc'_dim :: Int
                                 , autoenc'_We  :: Vector Float
                                 , autoenc'_b   :: Vector Float  
                                 , autoenc'_c1  :: Vector Float
                                 , autoenc'_c2  :: Vector Float
                                 } deriving Show

prepare :: Vector Float -> AutoEncoder
prepare v =
    let arr_We  = AIO.fromVectors (Z :. 100 :. 200) v
        arr_b   = AIO.fromVectors (Z :. 100) (V.slice 20000 100 v)
        arr_c1  = AIO.fromVectors (Z :. 100) (V.slice 20100 100 v)
        arr_c2  = AIO.fromVectors (Z :. 100) (V.slice 20200 100 v)
    in AutoEncoder 100 arr_We arr_b arr_c1 arr_c2

calcP :: AutoEncoder -> Array A.DIM0 Float -- Vector Float
calcP AutoEncoder {..} =
    {- AIO.toVectors . -} run . A.sum . A.map (tanh . (/ 100.0))  .  A.slice result $ (A.lift (Z :. All :. (0 :: Int)))
  where
        result = A.zipWith (+) (matMul (use autoenc_We) combined) blifted 
        combined = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_c1 A.++ use autoenc_c2)
        blifted  = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_b)

prepare' :: Vector Float -> AutoEncoder'
prepare' v =
    let arr_We  = V.slice 0 20000 v
        arr_c1  = V.slice 20000 100 v
        arr_c2  = V.slice 20100 100 v
        arr_b   = V.slice 20200 100 v        
    in AutoEncoder' 100 arr_We arr_b arr_c1 arr_c2

calcP' :: AutoEncoder' -> Float -- Array A.DIM0 Float -- Vector Float
calcP' AutoEncoder' {..} =
    V.sum . V.map (tanh . (/ 100.0)) $ V.zipWith (+) r autoenc'_b
  where
    c = autoenc'_c1 V.++ autoenc'_c2  
    r = mulMV (Mat (100,200) autoenc'_We) c

        {- flip V.map (V.fromList [0..99]) $ \i ->
          let v1 = V.slice (i*200) 200 (autoenc'_We)
          in V.sum $ V.zipWith (*) v1 c -}
        -- result = V.zipWith (+) (matMul (use autoenc_We) combined) blifted 
        -- combined = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_c1 A.++ use autoenc_c2)
        -- blifted  = A.replicate (A.lift $ Z :. All :. (1 :: Int)) (use autoenc_b)


