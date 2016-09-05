{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Numeric.LinearAlgebra hiding ((!))
import Numeric.LinearAlgebra.Devel
import Numeric.LinearAlgebra.SVD.SVDLIBC
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.IO.Class     (MonadIO(liftIO))
import           Control.Monad.Loops          (whileJust_)
import           Control.Monad.Trans          (lift) 
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict
import           Data.Conduit
import           Data.Conduit.Binary        (sourceFile)
import           Data.Conduit.List          (isolate)
import           Data.Conduit.Text   as CT
import           Data.Foldable              (forM_)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IORef                 
import           Data.List                  (foldl')
import           Data.Maybe                 (fromJust,fromMaybe)
import           Data.IntMap                (IntMap)
import qualified Data.IntMap         as IM
import qualified Data.Set            as Set
import           Data.Text                  (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy.IO   as TLIO
import           Data.Vector.Storable  (Vector, MVector(..),(!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Unboxed as VU

count :: (MonadIO m) => Int -> Sink a m ()
count !n =
  (await >>=) $ mapM_ $ \_ -> do
    when (n `mod` 10000 == 0) $ liftIO $ print n
    count (n+1)

enumerate :: (Monad m) => Int -> Conduit a m (Int,a)
enumerate !n = do
  mx <- await
  forM_ mx $ \x -> do  
    yield (n,x)
    enumerate (n+1)
 

buildMap (mref,mv) (dref,md) =
  whileJust_ await $ \(docid,t) -> do
    let ws = T.words t
    liftIO $ do
      is <-mapM (voca mref mv) ws
      let is' = Set.toList (Set.fromList is)
      doc dref md docid is'

voca mref mcount w = do
  BoundedWordMap n m mrev <- readIORef mref
  case HM.lookup w m of
    Nothing -> do
      MV.write mcount n 1
      writeIORef mref (BoundedWordMap (n+1) (HM.insert w n m) (IM.insert n w mrev))
      return n
    Just i -> MV.modify mcount (+1) i >> return i

doc :: IORef (IntMap [Int]) -> MVector (PrimState IO) Int -> Int -> [Int] -> IO ()
doc dref md docid js = do
  modifyIORef' dref (IM.insert docid js)
  mapM_ (MV.modify md (+1)) js  

tfidf :: Int -> IntMap [Int] -> Vector Int -> Int -> Int -> Double
tfidf sz dmap occ_dwt t d = fromMaybe 0 calc
  where calc = do ws <- IM.lookup d dmap
                  guard (t `elem` ws)
                  return $ log (fromIntegral sz/fromIntegral (occ_dwt!t))
  
data BoundedWordMap = BoundedWordMap { newid :: !Int
                                     , wordmap :: !(HashMap Text Int)
                                     , revmap :: !(IntMap Text) 
                                     }

emptyBWM = BoundedWordMap 0 HM.empty IM.empty
                      
main = do
  let sz = 100
  mref <- newIORef emptyBWM
  dref <- newIORef IM.empty
  mv <- V.thaw (V.replicate 1000000 (0 :: Int))
  md <- V.thaw (V.replicate 1000000 (0 :: Int))
  
  runResourceT $ 
    sourceFile "1M.training" =$= CT.decode CT.utf8 =$= CT.lines =$= isolate sz =$= enumerate 0 $$ getZipSink $
      (,) <$> ZipSink (count 0) <*> ZipSink (buildMap (mref,mv) (dref,md))
  --
  BoundedWordMap nword wmap rmap <- readIORef mref
  v <- V.freeze mv :: IO (Vector Int)
  occ_dwt <- V.freeze md :: IO (Vector Int)
  docs <- readIORef dref
  --
 
  mvvrows <- V.thaw (V.replicate 3000000 0)
  mvvcols <- V.thaw (V.replicate 3000000 0)
  mvvvals <- V.thaw (V.replicate 3000000 0)  
  
  cref <- newIORef (0 :: Int)
  forM_ [0..sz-1] $ \d -> do
    let ts = fromJust (IM.lookup d docs)
    c <- readIORef cref
    forM_ (zip [c,c+1..] ts) $ \(i,t) -> do
      MV.write mvvrows i (fromIntegral t)
      MV.write mvvcols i (fromIntegral d)
      MV.write mvvvals i (tfidf sz docs occ_dwt t d)
    writeIORef cref (c+length ts)
  ssz <- readIORef cref
  vvrows <- V.take ssz <$> V.freeze mvvrows
  vvcols <- V.take ssz <$> V.freeze mvvcols
  vvvals <- V.take ssz <$> V.freeze mvvvals

  let csr  = CSR vvvals vvcols vvrows nword sz
  let (_,dd,_) = sparseSvd 100 csr
  print csr
  print dd
  {-
  let v3 :: VU.Vector (Int,Int,Double)
        =  VU.take 100 $ (VU.zip3 (V.convert vvrows) (V.convert vvcols) (V.convert vvvals))
  print v3
  -}
  return ()
