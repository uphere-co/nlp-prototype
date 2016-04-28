{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Binary.Get
import           Data.Binary.IEEE754
import qualified Data.ByteString.Char8 as B
import           Data.Char           (chr, isSpace)
import           Data.Conduit        (($$),(=$=))
import qualified Data.Conduit        as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import           Data.Function       (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L
import           Data.Text           (Text(..))
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

import           System.IO
import           Unsafe.Coerce

skipSpace = CB.dropWhile (isSpace . unsafeCoerce)

getFloat = runGet getFloat32le <$> CB.take 4

getVector n = do
    w <- head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
    skipSpace    
    v <- replicateM n getFloat
    skipSpace
    return (TE.decodeUtf8 w,v)

normalize v =  
    let l2 = sum (map (\x->x*x) v)
    in map (\x->x/sqrt l2) v

cosDist v1 v2 = sum $ zipWith (*) v1 v2 


main :: IO ()
main = do
    putStrLn "parsing a result of word2vec program"
    runResourceT $ CB.sourceFile "vectors100.bin"
      $$ do
        r1 <- B.unpack . head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
        skipSpace
        r2 <- B.unpack . head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
        let nword = read r1 :: Int
            nvec  = read r2 :: Int
        skipSpace
        lst <- (map (\(x,(y,z))->(y,(x,z))) . zip [0..]) <$>
                 (replicateM nword $ ((,) <$> fst <*> normalize . snd) <$> getVector nvec)
        let m = HM.fromList lst
        case HM.lookup "test" m of
          Nothing -> liftIO $ print "test is not there"
          Just (i,vv) -> liftIO $ do
            putStrLn $ "index = " ++ show i
            let rs = map ((,) <$> fst <*> cosDist vv . snd . snd) . take 40 . L.sortBy (flip compare `on` (cosDist vv . snd . snd)) $ lst
            mapM_ print rs
        return ()
