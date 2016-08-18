{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NLP.RecursiveNN.AutoEncoder where

import           Control.Monad.IO.Class          ( liftIO )
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as HM
import           Data.MemoTrie
import qualified Data.Vector.Storable      as VS
import           Data.Vector.Storable            ( Vector )
import           Data.Vector.Storable.Matrix
import qualified LLVM.General.AST            as AST
import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.JIT       ( LLVMRunT, LLVMRun2T )
import           Symbolic.CodeGen.LLVM.Run
import           Symbolic.Differential           ( sdiff )
import           Symbolic.Eval                   ( seval )
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--
import           NLP.SyntaxTree.Type

data AENode = AENode { aenode_autoenc :: AutoEncoder
                     , aenode_c1  :: Vector Float
                     , aenode_c2  :: Vector Float
                     }
              
data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: Matrix Float
                               , autoenc_b   :: Vector Float
                               } 

ast :: (?expHash :: Exp Float :->: Hash) => AST.Module
ast = let idxi = ("i",1,100)
          idxj = ("j",1,100)
          exp1 :: MExp Float
          exp1 = add [ x_ [idxi], y_ [idxi] ]
      in mkAST exp1 [ V (mkSym "x") [idxi], V (mkSym "y") [idxj] ]

                   
encodeP :: AENode -> LLVMRun2T IO (Vector Float)
encodeP AENode {..} = do
  let vx = aenode_c1 -- VS.fromList [101..200]
      vy = aenode_c2 -- VS.fromList [201..300] :: VS.Vector Float
      vr = VS.replicate 100 0    :: VS.Vector Float
  mv@(VS.MVector _ fpr) <- liftIO $ VS.thaw vr
  runMain [vx,vy] fpr
  vr' <- liftIO $ VS.freeze mv
  return vr'
  
{- 
  VS.map tanh $ VS.zipWith (+) r b
  where
    we = autoenc_We aenode_autoenc
    b = autoenc_b aenode_autoenc
    c = aenode_c1 VS.++ aenode_c2  
    r = mulMV we c
-}


encode :: AutoEncoder
       -> BinTree (Vector Float)
       -> LLVMRun2T IO (BNTree (Vector Float) (Vector Float))
encode autoenc btr = go btr
  where go (BinNode x y) = do x' <- go x
                              y' <- go y
                              let vx = fromEither (rootElem x')
                                  vy = fromEither (rootElem y')
                                  ae = AENode autoenc vx vy
                              r <- encodeP ae
                              return (BNTNode r x' y')
        go (BinLeaf x) = return (BNTLeaf x)


{- 


data ADNode = ADNode { adnode_autodec :: AutoDecoder
                     , adnode_y  :: Vector Float
                     }

data AutoDecoder = AutoDecoder { autodec_dim :: Int
                               , autodec_Wd  :: Matrix Float
                               , autodec_b   :: Vector Float
                               }


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
-}
