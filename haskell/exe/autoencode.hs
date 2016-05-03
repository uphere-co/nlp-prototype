{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}


import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Char8      as B
import           Data.Foldable
import qualified Data.HashMap.Strict        as HM
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Vector.Storable              ((!), Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           System.Environment
-- 
import           Data.Vector.Storable.Matrix
import           NLP.RecursiveNN.AutoEncoder
import           NLP.SyntaxTree.Binarize
import           NLP.SyntaxTree.Parser
import           NLP.SyntaxTree.Regularize
import           NLP.SyntaxTree.Type
import           NLP.WordVector.Vectorize

 
prepareData :: IO (Vector Float)
prepareData = do
    bstr <- B.readFile "randomtest.dat"
    v :: Vector Float <- B.useAsCString bstr $ \cstr -> do
      nstr <- mallocBytes (400000000)
      copyBytes nstr cstr (400000000)
      fptr <- castForeignPtr <$> newForeignPtr_ nstr
      return (V.unsafeFromForeignPtr0 fptr 100000000)
    return v


getVectorizedTree :: WordVectorMap -> PennTree -> (BinTree Text, Maybe (BinTree (Vector Float)))
getVectorizedTree wvm tr = (btr, traverse (\w -> (fmap snd . HM.lookup w . wvmap) wvm) btr)
  where
    btr0  = binarizeR tr
    btr   = regularize btr0 -- . binarizeR) tr
    -- btr'  = fmap (\w -> (w, (fmap snd . HM.lookup w . wvmap) wvm)) btr
    -- btr'' = fmap (\case (w,Nothing) -> ("unknown",v_unknown) ; x -> x) btr'



main = do
    args <- getArgs
    let n1 = read (args !! 0) :: Int
        n2 = read (args !! 1) :: Int
    v <- prepareData
    let we = Mat (100,200) (V.slice 0 20000 v)
        be = V.slice 20000 100 v
        wd = Mat (200,100) (V.slice 20100 20000 v)
        bd = V.slice 40100 200 v
    let autoenc = AutoEncoder 100 we be
        autodec = AutoDecoder 100 wd bd
    txt <- TIO.readFile "LDC2003T05_parsed1.pos" -- "parsed.txt"
    (_,wvm) <- createWordVectorMap "vectors100statmt.bin" -- "vectors100t8.bin"
    let v_unknown = HM.lookup "unknown" (wvmap wvm)
        p' = penntree <* A.skipSpace 
        r = A.parseOnly (A.many1 p') txt
    case r of
      Left err -> print err
      Right lst -> do
        forM_ ((drop n1 . take n2) lst) $ \tr -> do
          let (btr,mvtr) = getVectorizedTree wvm tr
          forM_ mvtr $ \vtr ->
            print (encode autoenc vtr)

{- 
    let n = read (args !! 0) :: Int
    putStrLn "auto encoder test"
    v <- prepareData
    forM_ [0..n-1] $ \i -> do
      let autoenc = prepare' (V.slice i 20300 v)
      -- calcP autoenc `seq` (return ())
      -- calcP' autoenc `seq` return ()
      print (calcP' autoenc)
-}
