{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           System.IO
import qualified Data.HashMap.Strict as HM  (fromList, toList, lookup, map)
import qualified Data.Map            as M   (Map, delete, alter, empty, fromList, toList, lookup, map)
import           Text.Printf
import           Data.Maybe (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Text.Read                    (rational)
import qualified Data.Text.Encoding         as T.E
import qualified Data.Text.IO               as T.IO
import           Data.Monoid
import           Data.List                         (foldl')
import qualified Wikidata                   as W


--itemFile = "items.all"
itemFile = "items.uid"
-- itemFile = "items"
neFile   = "is_ne"

data Item = Item { name :: W.Name
                 , property :: W.P31 }

parseItemLine :: Text -> Item
parseItemLine line = parseItem (T.split (== '\t') line)
                   where 
                     parseItem [property, word] = Item (W.Name word) (W.P31 property)
                     parseItem tokens           = Item (W.Name (T.intercalate "_" tokens)) (W.P31 "ERROR")

newtype ItemsByKey key val = ItemsByKey  { items :: M.Map key [val] }
                           deriving (Show)

groupByItems :: [Item] -> ItemsByKey W.P31 W.Name
groupByItems items = ItemsByKey (foldl' update M.empty items) where 
                       update acc (Item n p31) = let f Nothing   = Just [n]
                                                     f (Just ns) = Just (n:ns)
                                                 in M.alter f p31 acc

serialize :: ItemsByKey key val -> [(key,[val])]
serialize (ItemsByKey items) = M.toList items

readItemsByKey :: FilePath -> IO (ItemsByKey W.P31 W.Name)
readItemsByKey itemFile = do
  itemsStr <- T.IO.readFile itemFile
  let items = map parseItemLine (T.lines itemsStr)
  return $ groupByItems items



parseNE   [word, tag] = (word, tag)
parseNE   _           = ("_ERROR_", "ERROR")
--parseNE   line        = error (show line)

main = do
  itemsByP31 <- readItemsByKey itemFile
  nesStr <- T.IO.readFile neFile  
  let 
      toTexts   = map (\(W.Name name)-> name)
      printable = map (\(W.P31 p31, names) -> p31 <> "\t" <> T.intercalate " " (toTexts names)) (serialize itemsByP31)
      
  mapM_  T.IO.putStrLn  printable
