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
import qualified Data.HashMap.Strict as HM
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
	m <- HM.fromList <$> (replicateM nword $ ((,) <$> fst <*> normalize . snd) <$> getVector nvec)
	
	liftIO $ print (HM.lookup "find" m) -- (last vs)
        return ()


