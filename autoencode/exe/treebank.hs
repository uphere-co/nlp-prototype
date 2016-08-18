{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable        as F
import qualified Data.HashMap.Strict  as HM
import qualified Data.List            as L
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Environment
--
import           NLP.SyntaxTree.Binarize
import           NLP.SyntaxTree.Parser
import           NLP.SyntaxTree.Printer
import           NLP.SyntaxTree.Regularize
import           NLP.WordVector.Vectorize

main :: IO ()
main = do
  args <- getArgs
  let n1 = read (args !! 0) :: Int
      n2 = read (args !! 1) :: Int
  txt <- TIO.readFile "LDC2003T05_parsed1.pos" -- "parsed.txt"
  (_,wvm) <- createWordVectorMap "vectors100statmt.bin" -- "vectors100t8.bin"
  let p' = penntree <* A.skipSpace 
  let r = A.parseOnly (A.many1 p') txt
  case r of
    Left err -> print err
    Right lst -> do
      result <- (\f -> foldM f (0::Int,0::Int,Set.empty) ((drop n1 . take n2) lst)) $ \acc tr -> do
        let btr0  = binarizeR tr
            btr   = regularize btr0
            btr'  = fmap (\w -> (w,HM.lookup w (wvmap wvm))) btr
            unknowns = map fst . filter (\case (_,Nothing) -> True; _ -> False) . F.toList $ btr'
            (passed,failed,unknownset) = acc
            (passed',failed')
              | null unknowns = (passed+1,failed)
              | otherwise     = (passed,failed+1)
             
        when ((not.null) unknowns) $ do
          TIO.putStrLn (pennTreePrint 0 tr)
          TIO.putStrLn (btreePrint [] (T.pack . show) btr0) 
          TIO.putStrLn (btreePrint [] (T.pack . show) btr)
        return (passed',failed',L.foldl' (\s x -> Set.insert x s) unknownset unknowns)
      print result
