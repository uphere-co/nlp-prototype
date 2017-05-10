{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NamedEntity where

import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Monoid
import           Data.List                         (foldl', all)

data NamedEntityClass = Org | Person | Loc | Time | Date | Other
                      deriving(Show, Eq)
data NamedEntity = NamedEntity { _str  :: Text
                               , _type :: NamedEntityClass}
                 deriving(Show, Eq)

parseStr :: Text -> Text -> NamedEntity
parseStr str t | t== "PERSON"   = NamedEntity str Person
               | t== "ORGANIZATION"      = NamedEntity str Org
               | t== "LOCATION" = NamedEntity str Loc
               | t== "TIME"     = NamedEntity str Time
               | t== "DATE"     = NamedEntity str Date
               | t== "O"        = NamedEntity str Other
parseStr _ _  = error "Unknown named entity class"


mergeToken :: NamedEntity -> NamedEntity -> NamedEntity
mergeToken (NamedEntity entity1 tag1) (NamedEntity entity2 tag2) | tag1 == tag2 = NamedEntity (T.unwords [entity1, entity2]) tag1
mergeToken _ _ = error "Cannot collapse entities with different types"

mergeTokensImpl :: NamedEntity -> [NamedEntity]
                -> ([NamedEntity] -> [NamedEntity])
                -> ([NamedEntity] -> [NamedEntity])
mergeTokensImpl entity [] outputBuilder = outputBuilder <> (entity:)
mergeTokensImpl current (next : unresolvedEntities) outputBuilder
    | _type current == _type next = mergeTokensImpl (mergeToken current next) unresolvedEntities outputBuilder
    | otherwise                   = mergeTokensImpl next unresolvedEntities (outputBuilder <> (current:) )

mergeTokens :: [NamedEntity] -> [NamedEntity]
mergeTokens []     = []
mergeTokens (e:es) = mergeTokensImpl e es id []
