{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WikiEntity where

import           Data.Text                         (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

newtype Name = Name { _name :: Text}
             deriving (Show, Eq, Ord)
newtype UID  = UID { _uid :: Text}
             deriving (Show, Eq, Ord)

parseEntityLine :: Text -> (UID, Name)
parseEntityLine str = (UID uid, Name name)
  where
    [uid, name] = T.split (=='\t') str


loadEntityReprs :: Text -> IO [(UID, Name)]
loadEntityReprs filename = do
    content <- T.IO.readFile "../rnn++/tests/data/wikidata.test.entities"
    let
      entities = map parseEntityLine (T.lines content)
    return entities

nameWords :: Name -> [Text]
nameWords (Name name) = T.words name
