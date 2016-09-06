#!/usr/bin/env runhaskell

import System.Environment
import System.IO

impstr = "foreign import ccall unsafe \"cliblinktest\" c_lib_link_test :: IO ()"

main = do
  args <- getArgs
  putStrLn "preprocessor"
  print (args !! 0)
  let ifile = args !! 1
      ofile = args !! 2

  withFile ifile ReadMode $ \hi -> do
    str <- hGetContents hi
    withFile ofile WriteMode $ \ho -> do
      hPutStrLn ho impstr
      hPutStr ho str

