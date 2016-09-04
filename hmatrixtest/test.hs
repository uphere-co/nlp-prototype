{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Numeric.LinearAlgebra hiding ((!))
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
import           Data.IntMap                (IntMap)
import qualified Data.IntMap         as IM
import qualified Data.Set            as Set
import           Data.Text                  (Text)
-- import qualified Data.Text.Lazy      as TL
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy.IO   as TLIO
import           Data.Vector.Storable  (Vector, MVector(..),(!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

a = (4><3) [ 1,2,3
           , 4,0,5
           , 7,7,2
           , 3,3,1 ] :: Matrix R


b = (4><2) [ 10, 1
           , 20, 2
           , 30, 3
           , 15, 1 ] :: Matrix R

x  = linearSolveLS a b



residual = norm_Frob (a <> x - b )

main' = do
  print a
  print b
  print x
  print residual

  print (singularValues a)


  a' <- randn 1000000 1000
  print (singularValues a')


count :: (MonadIO m) => Int -> Sink a m ()
count !n =
  (await >>=) $ mapM_ $ \_ -> do
    when (n `mod` 10000 == 0) $ do
      liftIO $ print n
      -- liftIO $ print t
    count (n+1)

enumerate :: (Monad m) => Int -> Conduit a m (Int,a)
enumerate !n = do
  mx <- await
  forM_ mx $ \x -> do  
    yield (n,x)
    enumerate (n+1)
  


wordCount !n = 
  (await >>=) $ maybe (return n) $ \t ->
    let ws = T.words t in wordCount (n+length ws)

wordHashMap !n !m = do
  (await >>=) $ maybe (return (n,m)) $ \t -> do
    let ws = T.words t
        (n',m') = foldl' update (n,m) ws
    wordHashMap n' m'
 where update (n,m) w = case HM.lookup w m of
                          Nothing -> (n+1,HM.insert w n m)
                          Just _ -> (n,m)

buildMap (mref,mv) (dref,md) =
  whileJust_ await $ \(docid,t) -> do
    let ws = T.words t
    liftIO $ do
      is <-mapM (voca mref mv) ws
      let is' = Set.toList (Set.fromList is)
      doc dref md docid is'

voca mref mv w = do
  BoundedWordMap n m <- readIORef mref
  case HM.lookup w m of
    Nothing -> do
      MV.write mv n 1
      writeIORef mref (BoundedWordMap (n+1) (HM.insert w n m))
      return n
    Just i -> MV.modify mv (+1) i >> return i

doc :: IORef (IntMap [Int]) -> MVector (PrimState IO) Int -> Int -> [Int] -> IO ()
doc dref md docid js = do
  modifyIORef' dref (IM.insert docid js)
  mapM_ (MV.modify md (+1)) js  


  
data BoundedWordMap = BoundedWordMap { newid :: !Int, wordmap :: !(HM.HashMap Text Int) }

emptyBWM = BoundedWordMap 0 HM.empty
                      
main = do
  mref <- newIORef emptyBWM
  dref <- newIORef IM.empty
  mv <- V.thaw (V.replicate 1000000 (0 :: Int))
  md <- V.thaw (V.replicate 1000000 (0 :: Int))
  
  runResourceT $ 
    sourceFile "1M.training" =$= CT.decode CT.utf8 =$= CT.lines =$= isolate 100000 =$= enumerate 0 $$ getZipSink $
      (,) <$> ZipSink (count 0) <*> ZipSink (buildMap (mref,mv) (dref,md))
  BoundedWordMap n m <- readIORef mref
  v <- V.freeze mv :: IO (Vector Int)
  d <- V.freeze md :: IO (Vector Int)
  docs <- readIORef dref
  
  case HM.lookup "the" m of
    Nothing -> print "no such word"
    Just i -> do
      -- print (v!i)
      putStrLn ("(v,d)" ++ show (v!i, d!i))

  print (IM.lookup 1000 docs) 

  {- 
  txt <- TLIO.readFile "1M.training" -- "/home/wavewave/repo/srcp/nlp-data/word2vec-dataset/1b.training"
  -- print txt

  let (n',mi,mc) = learnVocab txt
  print n'

  print $ M.lookup 2000 mc
  -}
