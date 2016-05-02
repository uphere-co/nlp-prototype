{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO(..), liftIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L
import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Vector.Storable as V
import           System.Environment        (getArgs)
import           System.IO
--
import           NLP.WordVector.Vectorize

main :: IO ()
main = do
    putStrLn "parsing a result of word2vec program"
    filename <- getArgs >>= \args -> return (args !! 0) 
    (lst,wvm) <- createWordVectorMap filename -- "vectors100statmt.bin"
    -- mapM_ (print . ((,) <$> fst <*> V.sum  . snd . snd))  (take 100 lst)
    --  let lst' = filter (\x -> let i = (fst . snd) x in i `elem` [75,76,77])  $ take 100 lst
    -- let lst' = take 1000 $ lst
    -- forM_ lst' $ \(w,(i,v)) -> do
    --   print (w,i)
    --   print v
    --   putStrLn ""
    -- print $ HM.lookup "time" (wvmap wvm)
    -- putStrLn ""
    -- print $ HM.lookup "so" (wvmap wvm) 
    forever (bot (lst,wvm))

bot (lst,wvm) = do    
    ln <- getLine
    lookupSimilarWord (lst,wvm) (T.pack (head (words ln)))
 


-- lookupSimilarWord :: (WordVectorMap -> Text -> IO ()
lookupSimilarWord (lst,wvm) word =
    case HM.lookup word (wvmap wvm) of
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
    
          
