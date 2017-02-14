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


{--
uidFile = "items.by_p31.test"
neFile   = "is_ne.test"
uidFile = "items.by_p31"
neFile   = "uid.is_ne"
--}
uidFile = "items.by_p31"
neFile   = "uid.is_ne"


newtype WikidataUID  = WikidataUID { unWikidataUID :: Text}
                     deriving (Show, Eq, Ord)
newtype NEFlag  = NEFlag { unNEFlag :: Text}
              deriving (Show, Eq, Ord)

data IsNE = IsNE { unIsNE :: M.Map WikidataUID NEFlag}
          deriving (Show)
 
parseIsNE [x,y] = (WikidataUID x, NEFlag y)


readIsNE :: FilePath -> IO IsNE 
readIsNE isNEFile = do                  
  neStr <- T.IO.readFile isNEFile
  return $ IsNE (M.fromList $ map (parseIsNE . T.split (=='\t')) (T.lines neStr))

isNE (IsNE neDict) uid = fromMaybe (NEFlag "Unknown") flag
                       where flag = M.lookup uid neDict


parseGroupedItems line = (tag, items)
                       where 
                         [tag, itemsStr] = T.split (=='\t') line
                         items = map WikidataUID (T.words itemsStr)


flagCount :: [NEFlag] -> M.Map NEFlag Int
flagCount = foldl' update M.empty
          where 
            update acc flag = let f Nothing  = Just 1
                                  f (Just n) = Just (n+1)
                              in M.alter f flag acc

ratioCutoff :: M.Map NEFlag Int -> NEFlag
ratioCutoff counts = f flag
                   where 
                     mTrue  = fromMaybe 0 $ M.lookup (NEFlag "True") counts
                     mFalse = fromMaybe 0 $ M.lookup (NEFlag "False") counts
                     flag = mTrue*5 > mFalse
                     f True = NEFlag "True"
                     f False = NEFlag "False"

reduceNEFlags :: [NEFlag] -> NEFlag
reduceNEFlags flags = let counts = flagCount flags 
                      in ratioCutoff counts

isNEProperty :: IsNE -> [WikidataUID] -> NEFlag
isNEProperty neDict items = f flag
                          where
                            flags = map (isNE neDict) items
                            flag  = any (== NEFlag "True") flags
                            f True = NEFlag "True"
                            f False = NEFlag "False"

main = do
  neDict <- readIsNE neFile
  
  {-
  print (isNE neDict "Q180925798172598712") -- "Unknown"
  print (isNE neDict "Q349") -- "False"
  print (isNE neDict "Q354") -- "True"
  -}

  groupedItemsStr <- T.IO.readFile uidFile
  let
      groups = map parseGroupedItems (T.lines groupedItemsStr)
      --ts = map (\(tag, items) -> tag <> "\t" <> isNEProperty neDict items) groups
      tmps = map (\(tag, items) -> (tag, reduceNEFlags (map (isNE neDict) items))) groups
      ts = map(\(tag, NEFlag flag)-> tag <> "\t" <> flag) tmps
  mapM_ T.IO.putStrLn ts

