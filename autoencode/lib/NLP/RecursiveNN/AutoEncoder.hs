{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NLP.RecursiveNN.AutoEncoder where

import           Control.Applicative             ( (<$>), (<*>), pure )
import           Control.Monad.IO.Class          ( liftIO )
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as HM
import           Data.MemoTrie
import qualified Data.Vector.Storable      as VS
import           Data.Vector.Storable            ( Vector )
import           Data.Vector.Storable.Matrix
import qualified LLVM.General.AST            as AST
import           LLVM.General.AST.Type           ( double )
import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.JIT       ( LLVMRunT )
import           Symbolic.CodeGen.LLVM.Operation ( external )
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

data ADNode = ADNode { adnode_autodec :: AutoDecoder
                     , adnode_y  :: Vector Float
                     }

data AutoDecoder = AutoDecoder { autodec_dim :: Int
                               , autodec_Wd  :: Matrix Float
                               , autodec_b   :: Vector Float
                               }

encodeExp :: (?expHash :: Exp Float :->: Hash) => (MExp Float, [Variable]) -- AST.Module
encodeExp =
  let idxi = ("i",1,100)
      idxj = ("j",1,100)
      idxk = ("k",1,100)
      idxI = ("I",1,200)
      c1 = ivar (mkSym "c1") [idxi]
      c2 = ivar (mkSym "c2") [idxj]
      we = ivar (mkSym "we") [idxk, idxI]
      be = ivar (mkSym "be") [idxk]

      c = concat_ idxI [c1,c2]
      prd = sum_ [idxI] (mul [we, c])
      result = tanh_ [ add [prd, be] ]
  in (result, [ V (mkSym "c1") [idxi]
              , V (mkSym "c2") [idxj]
              , V (mkSym "we")  [idxk,idxI]
              , V (mkSym "be")  [idxk] ] )

decodeExp :: (?expHash :: Exp Float :->: Hash) => (MExp Float, [Variable]) -- AST.Module
decodeExp =
  let idxi = ("i",1,100)
      idxj = ("j",1,100)
      idxk = ("k",1,100)
      idxI = ("I",1,200)
      y = ivar (mkSym "y") [idxk]
      wd = ivar (mkSym "wd") [idxI, idxk]
      bd = ivar (mkSym "bd") [idxI]
      prd = sum_ [idxI] (mul [wd, y])
      result = tanh_ [ add [prd, bd] ]
  in (result, [ V (mkSym "y") [idxk]
              , V (mkSym "wd") [idxI,idxk]
              , V (mkSym "bd") [idxI] ] )


fullAST :: (?expHash :: Exp Float :->: Hash) => AST.Module
fullAST = mkASTWithExt ext [("encode",encodeExp), ("decode",decodeExp)]
  where ext = external double "tanh" [(double, AST.Name "x")] 
 
                   
encodeP :: AENode -> LLVMRunT IO (Vector Float)
encodeP AENode {..} = do
  let vc1 = aenode_c1
      vc2 = aenode_c2
      vwe = mat_content (autoenc_We aenode_autoenc)
      vb  = autoenc_b aenode_autoenc
      vr = VS.replicate 100 0    :: VS.Vector Float
  mv@(VS.MVector _ fpr) <- liftIO $ VS.thaw vr
  callFn "encodeWrapper" [vc1,vc2,vwe,vb] fpr
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
       -> LLVMRunT IO (BNTree (Vector Float) (Vector Float))
encode autoenc btr = go btr
  where go (BinNode x y) = do x' <- go x
                              y' <- go y
                              let vx = fromEither (rootElem x')
                                  vy = fromEither (rootElem y')
                                  ae = AENode autoenc vx vy
                              r <- encodeP ae
                              return (BNTNode r x' y')
        go (BinLeaf x) = return (BNTLeaf x)


decodeP :: ADNode -> LLVMRunT IO (Vector Float, Vector Float)
decodeP ADNode {..} = do
  let dim = 100 --  autodec_dim adnode_autodec
      vy  = adnode_y
      vwd = mat_content (autodec_Wd adnode_autodec)
      vbd = autodec_b adnode_autodec
      vr = VS.replicate 200 0 :: VS.Vector Float
  mv@(VS.MVector _ fpr) <- liftIO $ VS.thaw vr
  callFn "decodeWrapper" [vy,vwd,vbd] fpr
  vr' <- liftIO $ VS.freeze mv
  let c1 = VS.slice 0 dim vr'
      c2 = VS.slice dim dim vr'
  return (c1,c2)

    -- r = mulMV wd adnode_y
    -- rc = V.map tanh $ V.zipWith (+) r b 
    -- c1 = V.slice 0 dim rc
    -- c2 = V.slice dim dim rc
   


 
decode :: AutoDecoder
       -> BNTree (Vector Float) ()
       -> LLVMRunT IO (BNTree (Vector Float) (Vector Float))
decode autodec bntr@(BNTNode v _ _) = go v bntr
  where 
    go v1 (BNTNode _ x y) = do (c1,c2) <- decodeP (ADNode autodec v1)
                               BNTNode v1 <$> go c1 x <*> go c2 y --  (go c1 x) (go c1 y)
    go v1 (BNTLeaf ()) = pure (BNTLeaf v1)
decode _ (BNTLeaf _) = error "shouldn't happen"


recDecode :: AutoDecoder
          -> BNTree (Vector Float) ()
          -> LLVMRunT IO (BNTree (BNTree (Vector Float) (Vector Float)) ())
recDecode _       (BNTLeaf ())      = pure (BNTLeaf ())
recDecode autodec n@(BNTNode _ x y) = 
  BNTNode <$> decode autodec n <*> recDecode autodec x <*> recDecode autodec y

