{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..), liftIO)
import           Control.Monad.Trans.Resource
import           Data.Binary.Get
import           Data.Binary.IEEE754
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Char8 as LB
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
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
-- import           Data.Vector.Storable.ByteString (byteStringToVector)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO
import           Unsafe.Coerce

skipSpace :: (MonadResource m) => C.Sink B.ByteString m ()
skipSpace = CB.dropWhile (isSpace . unsafeCoerce)

getVector :: (MonadResource m, MonadIO m) => Int -> C.Sink B.ByteString m (Text,Vector CFloat)
getVector n = do
    w <- head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
    skipSpace
    vbstr <- LB.toStrict <$> CB.take (4*n)
    v <- liftIO . B.useAsCString vbstr $ \cstr -> do
      nstr <- mallocBytes (4*n) 
      copyBytes nstr cstr (4*n)
      fptr {- :: ForeignPtr CFloat -} <- castForeignPtr <$> newForeignPtr_ nstr
      return (V.unsafeFromForeignPtr0 fptr n)
    skipSpace
    return (TE.decodeUtf8 w,v)

normalize v =  
    let l2 = V.sum (V.map (\x->x*x) v)
    in V.map (\x->x/sqrt l2) v

cosDist v1 v2 = V.sum $ V.zipWith (*) v1 v2 

main :: IO ()
main = do
    putStrLn "parsing a result of word2vec program"
    runResourceT $ CB.sourceFile "vectors100.bin" $$ do
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
