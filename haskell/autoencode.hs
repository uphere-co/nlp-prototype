{-# LANGUAGE ScopedTypeVariables #-}


import qualified Data.ByteString.Char8      as B
import           Data.Foldable
import           Data.Vector.Storable              ((!), Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr

-- 
import NLP.RecursiveNN.AutoEncoder

prepareData :: IO (Vector Float)
prepareData = do
    bstr <- B.readFile "randomtest.dat"
    v :: Vector Float <- B.useAsCString bstr $ \cstr -> do
      nstr <- mallocBytes (400000000)
      copyBytes nstr cstr (400000000)
      fptr <- castForeignPtr <$> newForeignPtr_ nstr
      return (V.unsafeFromForeignPtr0 fptr 100000000)
    return v

main = do
    putStrLn "auto encoder test"
    v <- prepareData
    forM_ [0..10] $ \i -> do
      let autoenc = prepare (V.slice (i*20400) ((i+1)*20400) v)
      print (calcP autoenc)
