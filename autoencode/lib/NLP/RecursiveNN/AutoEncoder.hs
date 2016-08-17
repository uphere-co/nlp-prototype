{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NLP.RecursiveNN.AutoEncoder where

import           Data.Vector.Storable              (Vector)
import qualified Data.Vector.Storable       as V
import           Data.Vector.Storable.Matrix
import           NLP.SyntaxTree.Type

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
    go v1 (BNTNode _ x y) = let ad     = ADNode autodec v1
                                (c1,_) = decodeP ad
                            in BNTNode v1 (go c1 x) (go c1 y)
    go v1 (BNTLeaf ()) = BNTLeaf v1
decode _ (BNTLeaf _) = error "shouldn't happen"


recDecode :: AutoDecoder -> BNTree (Vector Float) () -> BNTree (BNTree (Vector Float) (Vector Float)) ()
recDecode _       (BNTLeaf ())      = BNTLeaf ()
recDecode autodec n@(BNTNode _ x y) = BNTNode (decode autodec n) (recDecode autodec x) (recDecode autodec y) 
