{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NLP.RecursiveNN.NewAutoEncoder where

import           Control.Monad.IO.Class          ( liftIO )
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as HM
import           Data.MemoTrie
import qualified Data.Vector.Storable      as VS
import           Data.Vector.Storable            ( Vector )
import           Data.Vector.Storable.Matrix
import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.JIT       ( LLVMRunT )
import           Symbolic.CodeGen.LLVM.Run
import           Symbolic.Differential           ( sdiff )
import           Symbolic.Eval                   ( seval )
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--
import           NLP.SyntaxTree.Type


expfib' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (Int :->: MExp a) -> Int -> MExp a
expfib' _ 0 = varx
expfib' _ 1 = vary
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add [e1, e2]

expfib :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Int -> MExp a
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib

testfib :: IO ()
testfib = do
  let ?expHash = trie hash    
  let n = 5
      lexp1 = expfib n :: MExp Int
  prettyPrintR $ lexp1

test8 :: LLVMRunT IO (Either String ())
test8 = do
  let idxi = ("i",1,100)
      idxj = ("j",1,100)
  
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let exp1 :: MExp Float
      exp1 = add [ x_ [idxi], y_ [idxi] ]
  liftIO $ do
    putStr "f = "
    prettyPrintR exp1
  let ast = mkAST exp1 [ V (mkSym "x") [idxi]
                       , V (mkSym "y") [idxj]
                       ]
      vx = VS.fromList [101..200]
      vy = VS.fromList [201..300] :: VS.Vector Float
      vr = VS.replicate 100 0    :: VS.Vector Float
  runJITASTPrinter (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr


data AENode = AENode { aenode_autoenc :: AutoEncoder
                     , aenode_c1  :: Vector Float
                     , aenode_c2  :: Vector Float
                     }
              
data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: Matrix Float
                               , autoenc_b   :: Vector Float
                               } 

{- 
encodeP :: AENode -> LLVMRunT IO (Vector Float)
encodeP AENode {..} = VS.map tanh $ VS.zipWith (+) r b
  where
    we = autoenc_We aenode_autoenc
    b = autoenc_b aenode_autoenc
    c = aenode_c1 VS.++ aenode_c2  
    r = mulMV we c



encode :: AutoEncoder -> BinTree (Vector Float)
       -> LLVMRunT IO (BNTree (Vector Float) (Vector Float))
encode autoenc btr = go btr
  where go (BinNode x y) = let x' = go x
                               y' = go y
                               vx = fromEither (rootElem x')
                               vy = fromEither (rootElem y')
                               ae = AENode autoenc vx vy
                           in BNTNode (encodeP ae) x' y'
        go (BinLeaf x) = BNTLeaf x
-}

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
