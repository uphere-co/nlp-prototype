import Control.Applicative (many,some)
import System.Directory (getCurrentDirectory,getDirectoryContents)

import System.Environment (getArgs)
import System.FilePath (splitExtension)
import System.IO (hGetContents,hPutStrLn,stderr,stdin,stdout,withFile,IOMode(..))



import Parser


main0 = do
  print (parseOnly (char '1') "1sdlfjdslkjsd")

  print (parseOnly (many anyChar) "sldkfjfsldfjsdl")
  print (parseOnly (many anyChar) "")
  print (parseOnly (some anyChar) "")

  print (parseOnly (some anyChar) "abcd")

  print (parseOnly literal "\"sddlfkjsdlfjie\"")
  print (parseOnly (skipSpace >> literal) "       \"abcdsddlfkjsdlfjie\"")

  print (parseOnly (keyValue) "       \"key\"  : \"value\"   ")

  case (parse keyValue  "       \"key\"  : \"value") of
    Failed s -> print s
    Success a s -> print (a,s)
    Partial f ->
      case (f "slsl  \" ") of
        Failed s -> print s
        Success a s -> print (a,s)
        Partial f' -> print "partial"

  print (parseBuffer keyValue [ "       \"key\"  : \"value", "slsl  \" "] )

-- type FilePath = String

main' = do
  cwd <- getCurrentDirectory
  contents <- getDirectoryContents cwd
  let xs = map splitExtension contents
  print xs
  -- str <- readFile "monad.hs"
  -- putStrLn str
  {- 
  h <- openFile "monad.hs" ReadMode
  str' <- hGetContents h
  putStrLn str'
  hClose h
  -}
  withFile "monad.hs" ReadMode $ \h -> do
    str' <- hGetContents h
    putStrLn str'

  withFile "test.txt" WriteMode $ \h -> do
    hPutStrLn h "abcdefg"
    hPutStrLn stdout "I am uphere"
    hPutStrLn stderr "I am uphere 2"
  str <- hGetContents stdin
  putStrLn str


-- args  is [String]




main = do
  args <- getArgs
  let filename = args !! 0
  withFile filename ReadMode $ \h -> do
    str <- hGetContents h
    -- let result = parseOnly ((skipSpace *> (braces keyValue)) str
    let result = parseOnly object str

    print result

