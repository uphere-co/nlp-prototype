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

{-
uidFile = "aa"
neFile   = "p31.is_ne"
uidFile = "wikidata.names"
neFile   = "p31.is_ne"
-}
uidFile = "wikidata.names"
neFile   = "p31.is_ne"

data IsNE = IsNE { unIsNE :: M.Map W.P31 W.NETag}
          deriving (Show)
 
parseIsNE [x,y] = (W.P31 x, W.NETag y)


readIsNE :: FilePath -> IO IsNE 
readIsNE isNEFile = do                  
  neStr <- T.IO.readFile isNEFile
  return $ IsNE (M.fromList $ map (parseIsNE . T.split (=='\t')) (T.lines neStr))

isNE (IsNE neDict) p31 = fromMaybe ( W.NETag "Unknown") flag
                       where flag = M.lookup p31 neDict


parseEntity [uid,p31,name] = (uid, W.P31 p31, name)

main = do
  neDict <- readIsNE neFile
  entitiesStr <- T.IO.readFile uidFile
  let
      entities = map (parseEntity . T.split (=='\t')) (T.lines entitiesStr)
      ts = map(\(uid, p31, name)-> uid <>"\t"<> W.unTag (isNE neDict p31) <>"\t"<> name) entities
  mapM_ T.IO.putStrLn ts

