{-# LANGUAGE OverloadedStrings #-}

module Printer where

import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text as T  (concat,intercalate,replicate,take)
--
import Types

textprinter :: Int -> PennTree -> Text
textprinter n (PT _ lst) = T.intercalate "\n" (map (textprinter (n+4)) lst)
textprinter n (PN txt) = T.replicate n " " <> txt

treeprinter :: Int -> PennTree -> Text
treeprinter n (PT t lst) = "\n" <> fmttag <> T.concat (map (treeprinter (n+2)) lst)
  where fmttag = T.replicate n " " <> T.take 4 (t <> "    ") <> " "
treeprinter n (PN txt) = txt

btreeprinter :: Int -> BinTree Text -> Text
btreeprinter n (BinNode a b) = btreeprinter (n+2) a <> btreeprinter (n+2) b
btreeprinter n (BinLeaf txt) = T.replicate n " " <> txt <> "\n"
