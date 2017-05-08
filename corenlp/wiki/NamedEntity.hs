{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NamedEntity where

import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Monoid
import           Data.List                         (foldl', all)

data NamedEntity = Org    { _Org    :: Text}
                 | Person { _Person :: Text}
                 | Loc    { _Loc    :: Text}
                 | Time   { _Time   :: Text}
                 deriving(Show, Eq)

parseStr :: Text -> Text -> NamedEntity
parseStr str t | t== "PERSON"   = Person str
               | t== "ORG"      = Org str
               | t== "LOCATION" = Loc str
               | t== "TIME"     = Time str
parseStr _ _  = error "Unknown named entity class"

