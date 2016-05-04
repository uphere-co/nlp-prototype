{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- import           Criterion
import           Data.Array.Accelerate             (use, (:.)(..), Array(..), Z(..),
                                                    DIM1, DIM2, All(..), Acc, Exp)
import qualified Data.Array.Accelerate      as A
import           Data.Array.Accelerate.CUDA
import qualified Data.Array.Accelerate.IO   as AIO
import qualified Data.ByteString.Char8      as B
import qualified Data.List                  as L
import           Data.Vector.Storable              ((!), Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
-- import qualified Numeric.Sum                as N
import           System.Environment
import           System.Random.Mersenne
--
import           Data.Array.Accelerate.Matrix
import           Data.Vector.Storable.Matrix
import           Numeric.Kahan
--
-- import           Debug.Trace

 
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
  return v 

testVector v = print . kahanSumV $ V.zipWith (*) v v

testList v =
    let xs = V.toList v
    in print $ sum (zipWith (*) xs xs)

testAccel v = 
    let arr = AIO.fromVectors (Z :. 100000000) v :: Array DIM1 Float
    in print $ run $ A.sum $ A.zipWith (*) (use arr) (use arr)

testAccel2 v =
    let arr = AIO.fromVectors (Z :. 1000 :. 1000) v :: Array DIM2 Float
    in print $ run $ matTrace (matMul (use arr) (use arr)) -- use arr

testVector2 v =
    let n = 1000
        m = Mat (n,n) (V.take (n*n) v)
    in print $ traceM (m `mulMM` m)

testList2 :: Vector Float -> IO ()
testList2 v = 
    let n = 1000
        v' = V.take (n*n) v 
        v2 = V.fromList [ e | i <- [0..n-1], j <- [0..n-1],
                              let e = sum [ v' ! (i*n+k) * v' ! (k*n+j) | k <- [0..n-1] ] 
                        ] 
        t = sum [ v2 ! (i*n+i) | i <- [0..n-1] ]
    in print t

testAccel3 v =
    let arr = AIO.fromVectors (Z :. 1000 :. 1000) v :: Array DIM2 Float
    in print $ run $ matTrace (use arr `matMul` use arr `matMul` use arr `matMul` use arr) -- use arr

testVector3 v =
    let n = 1000
        m = Mat (n,n) (V.take (n*n) v)
    in print $ traceM (m `mulMM` m `mulMM` m `mulMM` m)

{- 
testList3 :: Vector Float -> IO ()
testList3 v = 
    let n = 1000
        v' = V.take (n*n) v 
        v2 = V.fromList [ e | i <- [0..n-1], j <- [0..n-1],
                              let e = sum [ v' ! (i*n+k) * v' ! (k*n+j) | k <- [0..n-1] ] 
                        ] 
        t = sum [ v2 ! (i*n+i) | i <- [0..n-1] ]
    in print t
-}

       
main = do
    args <- getArgs    
    v <- prepareData
    case args !! 0 of
      "accel" -> testAccel v
      "vector" -> testVector v
      "list" -> testList v

      "accel2"  -> testAccel2 v
      "vector2" -> testVector2 v
      "list2"   -> testList2 v  

      "accel3"  -> testAccel3 v
      "vector3" -> testVector3 v
