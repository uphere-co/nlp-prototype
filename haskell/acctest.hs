{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Array.Accelerate             (use, (:.)(..), Array(..), Z(..), DIM1)
import qualified Data.Array.Accelerate      as A
import           Data.Array.Accelerate.CUDA
import qualified Data.Array.Accelerate.IO   as AIO
import qualified Data.ByteString.Char8      as B
import           Data.Vector.Storable              (Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
-- import qualified Numeric.Sum                as N
import           System.Environment
import           System.Random.Mersenne


 
randomVector :: Int -> IO (Vector Float)
randomVector n = do
  g <- newMTGen Nothing
  V.replicateM n $ realToFrac <$> (random g :: IO Double)
     
createDataFile = do
  v <- randomVector 100000000
  V.unsafeWith v $ \ptr -> do
    let ptr' = castPtr ptr
    bstr <- B.packCStringLen (ptr',400000000)
    B.writeFile "randomtest.dat" bstr


prepareData :: IO (Vector Float)
prepareData = do
  bstr <- B.readFile "randomtest.dat"
  v :: Vector Float <- B.useAsCString bstr $ \cstr -> do
    nstr <- mallocBytes (400000000)
    copyBytes nstr cstr (400000000)
    fptr <- castForeignPtr <$> newForeignPtr_ nstr
    return (V.unsafeFromForeignPtr0 fptr 100000000)
  return v -- print 0

-- Naive summation is not good. We should reduce error using Kahan summation algorithm.

data KahanSum = KahanSum {-# UNPACK #-} !Float {-# UNPACK #-} !Float
              deriving (Eq, Show)

kahan (KahanSum s _) = s

kahanAdd (KahanSum sum c) x = KahanSum sum' c'
  where sum' = sum + y
        c'   = (sum' - sum) - y
        y    = x - c

kahanZero = KahanSum 0 0

kahanSum = kahan . V.foldl' kahanAdd kahanZero

testVector v = print . kahanSum $ V.zipWith (*) v v

  -- N.kbn . V.foldl' N.add N.zero $ V.zipWith (*) v v

testList v =
    let xs = V.toList v
    in print $ sum (zipWith (*) xs xs)

testAccel v = 
    let arr = AIO.fromVectors (Z :. 100000000) ((),v) :: Array DIM1 Float
    in print $ run $ A.sum $ A.zipWith (*) (use arr) (use arr)

main = do
    args <- getArgs    
    v <- prepareData
    case args !! 0 of
      "accel" -> testAccel v
      "vector" -> testVector v
      "list" -> testList v
