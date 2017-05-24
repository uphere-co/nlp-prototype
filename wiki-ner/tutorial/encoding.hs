{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import           Data.Char
import Data.Text
import qualified Data.Text    as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

main = do
  txt <- TIO.readFile "space"
  
  -- let txt = "abc\xc2\xa0abc"

  TIO.putStrLn txt
  print (T.words txt)
  print (TE.encodeUtf8 txt)

  print (T.split (== ' ') txt)

  print (T.split isSpace txt)
