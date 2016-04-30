{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable        as F
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text(..))
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
--
import           NLP.SyntaxTree.Binarize
import           NLP.SyntaxTree.Parser
import           NLP.SyntaxTree.Printer
import           NLP.SyntaxTree.Types

main :: IO ()
main = do
  txt <- TIO.readFile "LDC2003T05_parsed1.pos" -- "parsed.txt"
  let p' = penntree <* A.skipSpace 
  let r = A.parseOnly (A.many1 p') txt
  case r of
    Left err -> print err
    Right lst -> do
      F.forM_ (take 1 lst) $ \tr -> do
        print tr 
        putStrLn "-----"
        TIO.putStrLn (treeprinter 0 tr)
        putStrLn "-----"
        let btr = binarizeR tr
        print btr
        putStrLn "-----"
        TIO.putStrLn $ btreeprinter [] btr
        putStrLn "-----"
        let bntr = convert btr
        TIO.putStrLn  $ bntPrint [] id id bntr
        putStrLn "=====" 
        
