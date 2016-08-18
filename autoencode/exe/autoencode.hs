{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}


import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Char8      as B
import           Data.Foldable
import qualified Data.HashMap.Strict        as HM
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Vector.Storable              (Vector)
import qualified Data.Vector.Storable       as V
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           System.Environment
-- 
import           Data.Vector.Storable.Matrix
import           NLP.RecursiveNN.AutoEncoder
import           NLP.RecursiveNN.NewAutoEncoder
import           NLP.SyntaxTree.Binarize
import           NLP.SyntaxTree.Parser
import           NLP.SyntaxTree.Printer
import           NLP.SyntaxTree.Regularize
import           NLP.SyntaxTree.Type
import           NLP.WordVector.Vectorize

prepareData :: IO (Vector Float)
prepareData = do
    bstr <- B.readFile "randomtest.dat"
    v :: Vector Float <- B.useAsCString bstr $ \cstr -> do
      nstr <- mallocBytes (4000000)
      copyBytes nstr cstr (4000000)
      fptr <- castForeignPtr <$> newForeignPtr_ nstr
      return (V.unsafeFromForeignPtr0 fptr 1000000)
    return v

getVectorizedTree :: WordVectorMap -> PennTree -> (BinTree Text, Maybe (BinTree (Vector Float)))
getVectorizedTree wvm tr = (btr, traverse (\w -> (fmap snd . HM.lookup w . wvmap) wvm) btr)
  where
    btr0  = binarizeR tr
    btr   = regularize btr0

main' :: IO ()
main' = do
    args <- getArgs
    let n1 = read (args !! 0) :: Int
        n2 = read (args !! 1) :: Int
    v <- V.map (\x -> x - 0.5) <$> prepareData
    putStrLn "data prepared"
    let we = Mat (100,200) . V.slice 0 20000 . V.map (/100.0) $ v
        be = V.slice 20000 100 . V.map (/100.0) $ v
        wd = Mat (200,100) . V.slice 20100 20000 . V.map (/10.0) $ v
        bd = V.slice 40100 200 . V.map (/50.0) $ v
    let autoenc = AutoEncoder 100 we be
        autodec = AutoDecoder 100 wd bd
    txt <- TIO.readFile "LDC2003T05_parsed1.pos" -- "parsed.txt"
    (_,wvm) <- createWordVectorMap "vectors100statmt.bin" -- "vectors100t8.bin"
    let p' = penntree <* A.skipSpace 
        r = A.parseOnly (A.many1 p') txt
    case r of
      Left err -> print err
      Right lst -> do
        forM_ ((drop n1 . take n2) lst) $ \tr -> do
          let (_btr,mvtr) = getVectorizedTree wvm tr
          forM_ mvtr $ \vtr -> do
            let enc = encode autoenc vtr
                dec = decode autodec (fmap (const ()) enc)
            putStrLn "================"
            let printer :: BNTree (Vector Float) (Vector Float) -> Text
                printer = bntPrint [] (T.pack . show . V.take 4) (T.pack . show . V.take 4)

            TIO.putStrLn $ printer enc
            putStrLn "----------------"
            TIO.putStrLn $ printer dec
            let rdec = recDecode autodec (fmap (const ()) enc)
            TIO.putStrLn . bntPrint [] printer (const "") $ rdec

main :: IO ()
main = do
  testfib
