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
-- import           Data.Array.Accelerate.Matrix
import           Data.Vector.Storable.Matrix
import           NLP.SyntaxTree.Type


-- | this function is defined in HEAD of accelerate, but I define it here.
-- mkTanh x = A.Exp (A.PrimTanh A.floatingType `A.PrimApp` x)

{- 
data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: Array DIM2 Float
                               , autoenc_b   :: Array DIM1 Float  
                               , autoenc_c1  :: Array DIM1 Float
                               , autoenc_c2  :: Array DIM1 Float
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

-}

data AENode = AENode { aenode_autoenc :: AutoEncoder
                     , aenode_c1  :: Vector Float
                     , aenode_c2  :: Vector Float
                     }
              
data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: Matrix Float
                               , autoenc_b   :: Vector Float
                               } 

data ADNode = ADNode { adnode_autodec :: AutoDecoder
                     , adnode_y  :: Vector Float
                     }

data AutoDecoder = AutoDecoder { autodec_dim :: Int
                               , autodec_Wd  :: Matrix Float
                               , autodec_b   :: Vector Float
                               }
{- 
prepare' :: Vector Float -> AutoEncoder'
prepare' v =
    let arr_We  = V.slice 0 20000 v
        arr_c1  = V.slice 20000 100 v
        arr_c2  = V.slice 20100 100 v
        arr_b   = V.slice 20200 100 v        
    in AutoEncoder' 100 arr_We arr_b arr_c1 arr_c2
-}

encodeP :: AENode -> Vector Float
encodeP AENode {..} = V.map tanh $ V.zipWith (+) r b
  where
    we = autoenc_We aenode_autoenc
    b = autoenc_b aenode_autoenc
    c = aenode_c1 V.++ aenode_c2  
    r = mulMV we c

decodeP :: ADNode -> (Vector Float, Vector Float)
decodeP ADNode {..} = (c1,c2)
  where
    dim = autodec_dim adnode_autodec
    wd = autodec_Wd adnode_autodec 
    b = autodec_b adnode_autodec
    r = mulMV wd adnode_y
    rc = V.map tanh $ V.zipWith (+) r b 
    c1 = V.slice 0 dim rc
    c2 = V.slice dim dim rc
   
encode :: AutoEncoder -> BinTree (Vector Float) -> BNTree (Vector Float) (Vector Float)
encode autoenc btr = go btr
  where go (BinNode x y) = let x' = go x
                               y' = go y
                               vx = fromEither (rootElem x')
                               vy = fromEither (rootElem y')
                               ae = AENode autoenc vx vy
                           in BNTNode (encodeP ae) x' y'
        go (BinLeaf x) = BNTLeaf x


decode :: AutoDecoder -> BNTree (Vector Float) ()-> BNTree (Vector Float) (Vector Float)
decode autodec bntr@(BNTNode v _ _) = go v bntr
  where 
    go v (BNTNode _ x y) = let ad = ADNode autodec v
                               (c1,c2) = decodeP ad
                             in BNTNode v (go c1 x) (go c1 y)
    go v (BNTLeaf ()) = BNTLeaf v
decode autodec (BNTLeaf _) = error "shouldn't happen"

    
