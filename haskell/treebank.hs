{-# LANGUAGE OverloadedStrings #-}

-- import           Control.Applicative         ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable        as F
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text(..))
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
--
import           Binarize
import           Parser
import           Printer
import           Types



main :: IO ()
main = do
  txt <- TIO.readFile "parsed.txt"
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
        TIO.putStrLn $ btreeprinter [] btr -- (binarizeR tr)
        putStrLn "=====" 
        
