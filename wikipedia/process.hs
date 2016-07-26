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

  -- putStrLn "hello" = hPutStrLn stdout "hello"

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



{- 
printOrc :: Show a => Orc a -> IO ()
printOrc p = runOrc $ do
  x <- p
  liftIO $ putStrLn ("Ans = " ++ show x)
-}

-- f $ x = f x  
 
main = do
  let proc = shell "ls *.hs"
  result <- readCreateProcess proc ""
  let files = words result
  let mkhead f = shell ("sleep 1 && head " ++ f)

  printOrc $ do
    x1 <- liftIO (readCreateProcess (mkhead (files !! 0)) "")
          <|> liftIO (readCreateProcess (mkhead (files !! 1)) "")
          <|> liftIO (readCreateProcess (mkhead (files !! 2)) "")

    y <- return "b"
    return (x1,y)
  
