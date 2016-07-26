import Control.Monad.IO.Class (liftIO)

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
  let mkhead f = shell ("sleep 1 && head " ++ f ++ " | wc -l ")
  str <- readCreateProcess (mkhead f) ""
  print str
  
main = do
  let proc = shell "ls *.hs"
  result <- readCreateProcess proc ""
  let files = words result

  printOrc $ do
    let actions = map (liftIO . fileprocess) files
    -- foldr (+) 0 [1,2,3,4]
    --   = (1+(2+(3+(4+0)))) 
    -- foldr1 (+) [1,2,3,4]
    --   = (1+(2+(3+4)))
    -- foldr1 (<|>) [a1,a2,a3,a4]
    --   = (a1 <|> (a2 <|> (a3 <|> a4)))    
    foldr1 (<|>) actions 
    return ()
  

