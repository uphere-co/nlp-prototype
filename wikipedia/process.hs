import System.IO
import System.Process

main1 :: IO ()
main1 = do
  -- callCommand "ls" {- block comment -}

  let proc = shell "ls"

  -- output : (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph)
  (_, Just hout, _ ,_ ) <- createProcess proc { std_out = CreatePipe } 

  -- putStrLn "hello" = hPutStrLn stdout "hello"

  result <- hGetContents hout

  print (words result)
  return ()

main :: IO ()
main = do
  let proc = shell "ls *.hs"
  result <- readCreateProcess proc ""
  let files = words result
  let mkhead f = shell ("head " ++ f)
  strs <- mapM (\f -> readCreateProcess (mkhead f) "") files
  mapM_ print strs
  return ()