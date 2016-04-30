{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.IO.Class (MonadIO(..), liftIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L
--
import           NLP.WordVector.Vectorize

main :: IO ()
main = do
    putStrLn "parsing a result of word2vec program"
    (lst,wvm) <- createWordVectorMap "vectors100.bin"
    case HM.lookup "test" (wvmap wvm) of
      Nothing -> liftIO $ putStrLn "test is not there"
      Just (i,vv) -> liftIO $ do
        putStrLn $ "index = " ++ show i
        let vec = map ((,) <$> fst <*>  cosDist vv . snd . snd) $ lst
        let e = L.foldl' f [] vec
            f xs (s,d) | isNaN d       = xs
                       | otherwise     = g (s,d) xs
            g (s,d) []                 = [(s,d)]
            g (s,d) (x:xs) | d < snd x = x:g (s,d) xs
                           | otherwise = (s,d):x:xs
        mapM_ print (drop 1 $ take 40 e)
    
    {- 
    runResourceT $ CB.sourceFile "vectors100.bin" $$ do
      
      r1 <- B.unpack . head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
      skipSpace
      r2 <- B.unpack . head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
      let nword = read r1 :: Int
          nvec  = read r2 :: Int
      skipSpace
      lst <- (map (\(x,(y,z))->(y,(x,z))) . zip ([0..] :: [Int])) <$>
               (replicateM nword $ ((,) <$> fst <*> normalize . snd) <$> getVector nvec)
      let m = HM.fromList lst -}
          
