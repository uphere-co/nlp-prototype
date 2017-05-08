{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NamedEntity where

import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Monoid
import           Data.List                         (foldl', all)

data NamedEntityClass = Org | Person | Loc | Time
                      deriving(Show, Eq)
data NamedEntity = NamedEntity { _str  :: Text
                               , _type :: NamedEntityClass}
                 deriving(Show, Eq)

parseStr :: Text -> Text -> NamedEntity
parseStr str t | t== "PERSON"   = NamedEntity str Person
               | t== "ORG"      = NamedEntity str Org
               | t== "LOCATION" = NamedEntity str Loc
               | t== "TIME"     = NamedEntity str Time
parseStr _ _  = error "Unknown named entity class"


