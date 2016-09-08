{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO(..), liftIO)
import           Control.Monad.ST
import           Data.Function             (on)
import qualified Data.HashMap.Strict as HM
import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable as V
import qualified Data.Vector as VB
import           Foreign.Ptr
import           Foreign.Storable
import           System.Environment        (getArgs)
--
import           NLP.WordVector.Vectorize

instance (Storable a, Storable b) => Storable (a,b) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: b)
  alignment _  = 8
  peek ptr = peek (castPtr ptr) >>= \i -> peek (castPtr (ptr `plusPtr` sizeOf i)) >>= \f -> return (i,f)
  poke ptr (i,f) = poke (castPtr ptr) i >> poke (castPtr (ptr `plusPtr` sizeOf i)) f 

main :: IO ()
main = do
    putStrLn "parsing a result of word2vec program"
    filename <- getArgs >>= \args -> return (args !! 0) 
    (lst,wvm) <- createWordVectorMap filename
    let namemap = VB.fromList $ map fst lst -- this is very fragile
    forever (bot (namemap,lst,wvm))

bot :: (VB.Vector Text, [(Text, (Int, V.Vector Float))], WordVectorMap) -> IO ()
bot (nm,vec,wvm) = do    
    ln <- getLine
    lookupSimilarWord (nm,vec,wvm) (T.pack (head (words ln)))
 
lookupSimilarWord :: (VB.Vector Text,[(Text,(Int,V.Vector Float))],WordVectorMap) -> Text -> IO ()
lookupSimilarWord (namemap,lst,wvm) word =
    case HM.lookup word (wvmap wvm) of
      Nothing -> liftIO $ putStrLn "test is not there"
      Just (i,vv) -> liftIO $ do
        putStrLn $ "index = " ++ show i
        let v = runST $ do
                  vec'  <- V.thaw . V.fromList . map ((,) <$> fst . snd <*> cosDist vv . snd . snd) $ lst
                  VA.partialSortBy (flip compare `on` snd) vec' 40
                  V.freeze vec' 
        mapM_ print $ map (\(j,d) -> (namemap VB.! j,d)) $ V.toList . V.take 40 $ v
          
