{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module NLP.RecursiveNN.AutoEncoder where

import           Control.Monad.IO.Class          ( liftIO )
import           Data.MemoTrie
import qualified Data.Vector.Storable      as VS
import           Data.Vector.Storable            ( Vector )
import           Data.Vector.Storable.Matrix
import qualified LLVM.General.AST            as AST
import           LLVM.General.AST.Type           ( double )
--
import           Symbolic.CodeGen.LLVM.JIT       ( LLVMRunT )
import           Symbolic.CodeGen.LLVM.Operation ( external )
import           Symbolic.CodeGen.LLVM.Run
import           Symbolic.Predefined
import           Symbolic.Type
--
import           NLP.SyntaxTree.Type

type WVector = Vector Float
type family WVal a :: *
type instance WVal WVector = Float

type WMatrix = Matrix Float
type WExp = Exp Float
type WMExp = MExp Float

data AENode = AENode { aenode_autoenc :: AutoEncoder
                     , aenode_c1  :: WVector
                     , aenode_c2  :: WVector
                     }
              
data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: WMatrix
                               , autoenc_b   :: WVector
                               } 

data ADNode = ADNode { adnode_autodec :: AutoDecoder
                     , adnode_y  :: WVector
                     }

data AutoDecoder = AutoDecoder { autodec_dim :: Int
                               , autodec_Wd  :: WMatrix
                               , autodec_b   :: WVector
                               }

encodeExp :: (?expHash :: WExp :->: Hash) => (WMExp, [Variable])
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

decodeExp :: (?expHash :: WExp :->: Hash) => (WMExp, [Variable])
decodeExp =
  let idxk = ("k",1,100)
      idxI = ("I",1,200)
      y = ivar (mkSym "y") [idxk]
      wd = ivar (mkSym "wd") [idxI, idxk]
      bd = ivar (mkSym "bd") [idxI]
      prd = sum_ [idxk] (mul [wd, y])
      result = tanh_ [ add [prd, bd] ]
  in (result, [ V (mkSym "y") [idxk]
              , V (mkSym "wd") [idxI,idxk]
              , V (mkSym "bd") [idxI] ] )


fullAST :: (?expHash :: WExp :->: Hash) => AST.Module
fullAST = mkASTWithExt ext [("encode",encodeExp), ("decode",decodeExp)]
  where ext = external double "tanh" [(double, AST.Name "x")] 
 
                   
encodeP :: AENode -> LLVMRunT IO WVector
encodeP AENode {..} = do
  let vc1 = aenode_c1
      vc2 = aenode_c2
      vwe = mat_content (autoenc_We aenode_autoenc)
      vb  = autoenc_b aenode_autoenc
      vr = VS.replicate 100 0    :: WVector
  mv@(VS.MVector _ fpr) <- liftIO $ VS.thaw vr
  callFn "encode" [vc1,vc2,vwe,vb] fpr
  vr' <- liftIO $ VS.freeze mv
  return vr'
  
encode :: AutoEncoder
       -> BinTree WVector
       -> LLVMRunT IO (BNTree WVector WVector)
encode _ (BinLeaf x) = pure (BNTLeaf x)
encode autoenc (BinNode x y) = do
    x' <- encode autoenc x
    y' <- encode autoenc y
    r <- node x' y'
    return (BNTNode r x' y')
  where
    node x' y' = let vx = fromEither . rootElem $ x'
                     vy = fromEither . rootElem $ y'
                     ae = AENode autoenc vx vy
                 in encodeP ae


decodeP :: ADNode -> LLVMRunT IO (WVector, WVector)
decodeP ADNode {..} = do
  let dim = 100 --  autodec_dim adnode_autodec
      vy  = adnode_y
      vwd = mat_content (autodec_Wd adnode_autodec)
      vbd = autodec_b adnode_autodec
      vr = VS.replicate 200 0 :: WVector
  mv@(VS.MVector _ fpr) <- liftIO $ VS.thaw vr
  callFn "decode" [vy,vwd,vbd] fpr
  vr' <- liftIO $ VS.freeze mv
  let c1 = VS.slice 0 dim vr'
      c2 = VS.slice dim dim vr'
  return (c1,c2)

decode :: AutoDecoder
       -> BNTree WVector e
       -> LLVMRunT IO (BNTree WVector WVector)
decode autodec bntr@(BNTNode v _ _) = go v bntr
  where 
    go v1 (BNTNode _ x y) = do
       (c1,c2) <- decodeP (ADNode autodec v1)
       BNTNode v1 <$> go c1 x <*> go c2 y
    go v1 (BNTLeaf _) = pure (BNTLeaf v1)
decode _ (BNTLeaf _) = error "shouldn't happen"

-- Binary tree with child-tree-valued nodes !! (sort of)
recDecode :: AutoDecoder
          -> BNTree WVector e
          -> LLVMRunT IO (BNTree (BNTree WVector WVector) ())
recDecode _       (BNTLeaf _)      = pure (BNTLeaf ())
recDecode autodec n@(BNTNode _ x y) = 
  BNTNode <$> decode autodec n <*> recDecode autodec x <*> recDecode autodec y


-- Tree manipulation functions

-- zipTree - error occurs when the structures don't match 
zipTree :: BNTree a1 e1
          -> BNTree a2 e2
          -> BNTree (a1,a2) (e1,e2)
zipTree (BNTLeaf n1) (BNTLeaf n2) = BNTLeaf (n1,n2)
zipTree (BNTNode n1 x1 y1) (BNTNode n2 x2 y2) =
    let tx = zipTree x1 x2
        ty = zipTree y1 y2
    in BNTNode (n1,n2) tx ty 
zipTree _ _ = error "zipTree : invalid input" 

-- zipWithTree
zipWithTree :: (a1 -> a2 -> c)
          -> BNTree a1 a1
          -> BNTree a2 a2
          -> BNTree c c
zipWithTree f (BNTLeaf n1) (BNTLeaf n2) = BNTLeaf $ f n1 n2
zipWithTree f (BNTNode n1 x1 y1) (BNTNode n2 x2 y2) =
    let xbnt = zipWithTree f x1 x2
        ybnt = zipWithTree f y1 y2
    in BNTNode ( f n1 n2 ) xbnt ybnt 
zipWithTree f (BNTLeaf n1) (BNTNode n2 _ _) = BNTLeaf $ f n1 n2
zipWithTree f (BNTNode n1 _ _) (BNTLeaf n2) = BNTLeaf $ f n1 n2

-- zipWithLeaf - works only for the same structures, otherwise raises an error
zipWithLeaf :: (e1 -> e2 -> c)
          -> BNTree a1 e1
          -> BNTree a2 e2
          -> BNTree () c
zipWithLeaf f (BNTLeaf n1) (BNTLeaf n2) = BNTLeaf $ f n1 n2
zipWithLeaf f (BNTNode _ x1 y1) (BNTNode _ x2 y2) =
    let xbnt = zipWithLeaf f x1 x2
        ybnt = zipWithLeaf f y1 y2
    in BNTNode () xbnt ybnt 
zipWithLeaf _ _ _ = error "shouldn't happen"

-- foldNode - ignore leaf values
foldNode :: (a -> a -> a) -> a -> BNTree a e -> a
foldNode _ a (BNTLeaf _)  = a
foldNode f a (BNTNode _ x y)  = let vx = foldNode f a x
                                    vy = foldNode f a y
                                 in f vx vy

-- foldLeaf - fold only on leaves
foldLeaf :: (e -> e -> e) -> e -> BNTree a e -> e
foldLeaf f e (BNTNode _ x y)  = f (foldLeaf f e x) (foldLeaf f e y)
foldLeaf f e (BNTLeaf d)  = f e d

-- mapTree
mapTree :: (a -> b) -> BNTree a a -> BNTree b b
mapTree f (BNTLeaf a) = BNTLeaf $ f a
mapTree f (BNTNode a x y) = let x' = mapTree f x
                                y' = mapTree f y
                            in BNTNode (f a) x' y'

-- Compute L^2 norm
l2RAE:: AutoEncoder
          -> AutoDecoder
          -> BinTree WVector
          -> LLVMRunT IO (WVal WVector)
l2RAE ae ad bt  = do
     bte <- encode ae bt
     btd <- decode ad bte
     let l2tree::BNTree (WVal WVector) (WVal WVector)
         l2tree = zipWithTree l2 bte btd
     return $ foldNode (+) 0 l2tree
  where
    l2 :: WVector -> WVector -> WVal WVector
    l2 v1 v2 = let vec_sub = VS.zipWith (*) v1 v2
               in VS.sum $ VS.zipWith (*) vec_sub vec_sub

-- unfolding RAE L^2 norm for a node
l2unfoldingRAE_node:: BNTree a WVector
                   -> BNTree b WVector
                   -> WVal WVector
l2unfoldingRAE_node bt1 bt2  = 
    let l2tree::BNTree () (WVal WVector)
        l2tree = zipWithLeaf l2 bt1 bt2
    in foldLeaf (+) 0 l2tree
  where
    l2 :: WVector -> WVector -> WVal WVector
    l2 v1 v2 = let vec_sub = VS.zipWith (*) v1 v2
               in VS.sum $ VS.zipWith (*) vec_sub vec_sub

-- unfolding RAE L^2 norm
l2unfoldingRAE:: AutoEncoder
          -> AutoDecoder
          -> BinTree WVector
          -> LLVMRunT IO (WVal WVector)
l2unfoldingRAE ae ad bt  = do
     bte <- encode ae bt
     btd <- decode ad bte
     return $ go bte btd
  where
    go x@(BNTNode _ xa xb) y@(BNTNode _ ya yb) =
        let l = l2unfoldingRAE_node x y
            la = go xa ya
            lb = go xb yb
        in l+la+lb
    go (BNTLeaf _) (BNTLeaf _) = 0
    go _ _ = error "l2unfoldingRAE : Different tree structures (something went wrong)"
