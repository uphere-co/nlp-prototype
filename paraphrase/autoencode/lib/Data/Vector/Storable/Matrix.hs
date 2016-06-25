module Data.Vector.Storable.Matrix where

import Data.Vector.Storable (Vector, Storable(..) )
import qualified Data.Vector.Storable as V



data Matrix a = Mat { mat_size :: (Int,Int)
                    , mat_content :: Vector a }
               
mat_rows = fst . mat_size

mat_cols = snd . mat_size


to1Dindex :: Matrix a -> (Int,Int) -> Int
to1Dindex (Mat (r,c) _) (i,j) = i*c+j

to2Dindex :: Matrix a -> Int -> (Int,Int)
to2Dindex (Mat (r,c) _) i = i `divMod` c

at :: (Storable a) => Matrix a -> (Int,Int) -> a
at m (i,j) = mat_content m V.! to1Dindex m (i,j)

rowVector :: (Storable a) => Int -> Matrix a -> Vector a
rowVector i (Mat (r,c) m) = V.slice (i*c) c m


dotV :: Vector Float -> Vector Float -> Float
dotV v1 v2 = V.sum $ V.zipWith (*) v1 v2

mulMV :: Matrix Float -> Vector Float -> Vector Float
mulMV m v =
    flip V.map (V.fromList [0..rows-1]) $ \i ->
      let v1 = rowVector i m -- V.slice (i*cols) cols (mat_content m)
      in dotV v1 v
  where (rows,cols) = mat_size m

mulMM :: Matrix Float -> Matrix Float -> Matrix Float
mulMM m1 m2 = Mat (rows1,cols2) r
  where (rows1,cols1) = mat_size m1
        (rows2,cols2) = mat_size m2
        r = V.generate (rows1*cols2) $ \k ->
              let (i,j) = k `divMod` cols2
                  v1 = rowVector i m1 -- V.slice (i*cols1) cols1 (mat_content m1)
                  v2 = rowVector j (transposeM m2) -- V.slice (j*rows2) rows2 (mat_content (transposeM m2))
              in v1 `dotV` v2

traceM :: Matrix Float -> Float
traceM m = V.sum (V.generate (mat_rows m) $ \k -> m `at` (k,k))

transposeM :: Matrix Float -> Matrix Float
transposeM m@(Mat (rows,cols) _) = Mat (cols,rows) m'
  where m' = V.generate (rows*cols) $ \k -> let (i,j) = k `divMod` cols in m `at` (j,i)
