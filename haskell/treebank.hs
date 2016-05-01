{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable        as F
import qualified Data.HashMap.Strict  as HM
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Vector.Storable as V
--
import           NLP.SyntaxTree.Binarize
import           NLP.SyntaxTree.Parser
import           NLP.SyntaxTree.Printer
import           NLP.SyntaxTree.Regularize
import           NLP.WordVector.Vectorize

main :: IO ()
main = do
  txt <- TIO.readFile "LDC2003T05_parsed1.pos" -- "parsed.txt"
  (_,wvm) <- createWordVectorMap "vectors100.bin"
  let p' = penntree <* A.skipSpace 
  let r = A.parseOnly (A.many1 p') txt
  case r of
    Left err -> print err
    Right lst -> do
      F.forM_ (take 100 lst) $ \tr -> do
        print tr 
        putStrLn "-----"
        TIO.putStrLn (treeprinter 0 tr)
        putStrLn "-----"
        let btr = (regularize . binarizeR) tr
        TIO.putStrLn $ btreePrint [] id btr
        putStrLn "-----"
        let btr' = flip fmap btr (\w -> (w,HM.lookup w (wvmap wvm)))
        TIO.putStrLn $ btreePrint [] f btr'
          where f (w,(Just r)) = w <> " : " <> (T.pack . show . V.sum . snd) r
                f (w,Nothing)  = w <> " : no such word"
{-              
        putStrLn "-----"
        let bntr = convert btr
        TIO.putStrLn  $ bntPrint [] id id bntr
        putStrLn "=====" 
        
-}
