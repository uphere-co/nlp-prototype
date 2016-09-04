{-# LANGUAGE BangPatterns #-}

import Numeric.LinearAlgebra
import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO(liftIO))
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.Text   as CT
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List                  (foldl')
import           Data.IntMap                (IntMap)
import qualified Data.IntMap         as M
import           Data.Text                  (Text)
-- import qualified Data.Text.Lazy      as TL
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy.IO   as TLIO 

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

{- 
learnVocab :: TL.Text -> (Int,HashMap Text Int, IntMap Int)
learnVocab txt = foldl' addcount (0,HM.empty,M.empty) wss 
  where ls = TL.lines txt
        wss = map TL.toStrict .  concatMap TL.words $ ls
        addcount !(n,mi,mc) !k = case HM.lookup k mi of
                                   Nothing -> (n+1,HM.insert k n mi,M.insert n 1 mc)
                                   Just i -> (n,mi,M.update (Just . (+1)) i mc)
-}

count :: (MonadIO m) => Int -> Sink Text m ()
count !n = do
  mt <- await
  case mt of
    Nothing -> return ()
    Just t -> do
      when (n `mod` 10000 == 0) $ do
        liftIO $ print n
        -- liftIO $ print t
      count (n+1)


wordCount !n = do
  mt <- await
  case mt of
    Nothing -> return n
    Just t -> do
      let ws = T.words t
      wordCount (n+length ws)

-- wordMap w 


main = do
  r <- runResourceT $
    sourceFile "1M.training" =$= CT.decode CT.utf8 =$= CT.lines $$ getZipSink $ (,) <$> ZipSink (count 0) <*> ZipSink (wordCount 0)
  print r
  {- 
  txt <- TLIO.readFile "1M.training" -- "/home/wavewave/repo/srcp/nlp-data/word2vec-dataset/1b.training"
  -- print txt

  let (n',mi,mc) = learnVocab txt
  print n'

  print $ M.lookup 2000 mc
  -}
