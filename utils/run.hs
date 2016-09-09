import Data.Foldable
import System.Process

main :: IO ()
main = do
  forM_ [1..12] $ \n -> 
    runCommand ("./parse.sh /home/wavewave/repo/srcp/topic-modeling-for-US-patents/haskell/sentences" ++ show n ++ ".txt > LDC2003T05_parsed" ++ show n ++ ".pos")
  return ()
