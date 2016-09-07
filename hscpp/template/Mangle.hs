-- {-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Data.Conduit.Shell

import System.Directory
import System.FilePath

-- echo "int f1(char *, int) {} " | g++ -x c++ -S - -o- | grep "^_.*:$" | sed -e 's/:$//'

proc1 = shell "echo \"int f1(char *, int) {} \""
proc2 f = shell ("g++ -x c++ -o - " ++ f)
proc3 = shell "grep \"^_.*:$\""
proc4 = shell "sed -e 's/:$//'"


main = do
  tdir <- getTemporaryDirectory
  let f = tdir </> "test.cc"
  writeFile f "#include <vector> \ntemplate void printout<int> (std::vector<int>*);"
  -- let echo str = shell ("echo '" ++ str ++ "'")
  run (proc2 f ) -- $|  conduit (do Just x <- await; yield x) ) -- (echo ";void printout<int> (std::vector<int>*) {}" $| proc2 $| proc3) --  $| proc4)
