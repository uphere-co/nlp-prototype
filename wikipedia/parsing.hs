import Control.Monad.IO.Class (liftIO)
import Data.List.Split (chunksOf)
import Orc
import System.IO
import System.Process

main1 :: IO ()
main1 = do
  -- callCommand "ls" {- block comment -}

  let proc = shell "ls"
  -- output : (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph)
  (_, Just hout, _ ,_ ) <- createProcess proc { std_out = CreatePipe } 

  result <- hGetContents hout
  print (words result)
  return ()

main_seq :: IO ()
main_seq = do
  let proc = shell "ls *.hs"
  result <- readCreateProcess proc ""
  let files = words result
  let mkhead f = shell ("sleep 1 && head " ++ f)
  strs <- mapM (\f -> readCreateProcess (mkhead f) "") files
  mapM_ print strs
  return ()

fileprocess f = do
  let mkhead f = shell ("ipython stanford.py " ++ f ++ " && ipython bllip.py " ++ f )
  str <- readCreateProcess (mkhead f) ""
  print str

ncores = 20

main = do
  let proc = shell "ls ~/word2vec/1b/1b.training.short_sentences.known.*"
  result <- readCreateProcess proc ""
  let files = words result

  printOrc $ do
    let actions = map (liftIO . fileprocess) files
        n = length actions
        d = n `div` ncores 
        m = if n `mod` ncores == 0 then 0 else 1
        actionss = chunksOf (d + m) actions 
    let merged = map sequence actionss
    foldr1 (<|>) merged                   -- P [ S [ a,b,c ] , S [d,e,f ] ]  
    return ()
  

