{-# LANGUAGE OverloadedStrings #-}
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


--itemFile = "items.all"
itemFile = "items.10k"
neFile   = "nes.all"


parseItem [property, word] = (word, property)
parseItem _                = ("_ERROR_", "ERROR")
-- parseItem line             = error (show line)

parseNE   [word, tag] = (word, tag)
parseNE   _           = ("_ERROR_", "ERROR")
--parseNE   line        = error (show line)


update acc (n, p31) = let f Nothing   = Just [n]
                          f (Just ns) = Just (n:ns)
                      in M.alter f p31 acc

groupByItems = foldl' update M.empty

main = do
  itemsStr <- T.IO.readFile itemFile
  print "Items are loaded"
  nesStr <- T.IO.readFile neFile
  print "NEs are loaded"
  
  let 
      items     = map (parseItem . T.split (== '\t')) (T.lines itemsStr)
      itemByP31 = groupByItems items
      
      itemsByP31 = M.toList itemByP31

      printable = map (\(p31, names) -> p31 <> "\t" <> T.intercalate " " names) itemsByP31
  mapM_  T.IO.putStrLn  printable
  