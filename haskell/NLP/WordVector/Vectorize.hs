module NLP.WordVector.Vectorize where

import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as B
import           Data.Char           (isSpace)
import           Data.Conduit        (($$),(=$=))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import qualified Data.HashMap.Strict as HM
import           Data.Text            (Text)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import           Unsafe.Coerce
--
import           NLP.WordVector.Parser

data WordVectorMap = WVMap { wvdim :: Int 
                           , wvmap :: HM.HashMap Text (Int,Vector Float) }
                     
cosDist :: Vector Float -> Vector Float -> Float
cosDist v1 v2 = V.sum $ V.zipWith (*) v1 v2 
                     
createWordVectorMap :: FilePath -> IO ([(Text,(Int,Vector Float))] ,WordVectorMap)
createWordVectorMap fp =
    runResourceT $ CB.sourceFile fp $$ do
      r1 <- B.unpack . head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
      skipSpace
      r2 <- B.unpack . head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
      let nword = read r1 :: Int
          nvec  = read r2 :: Int
      skipSpace
      lst <- (map (\(x,(y,z))->(y,(x,z))) . zip ([0..] :: [Int])) <$>
               (replicateM nword $ ((,) <$> fst <*> {- normalize . -} snd) <$> getVector nvec)
      return (lst,WVMap nvec (HM.fromList lst))
