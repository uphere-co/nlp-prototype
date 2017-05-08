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


mergeToken :: NamedEntity -> NamedEntity -> NamedEntity
mergeToken (NamedEntity entity1 tag1) (NamedEntity entity2 tag2) | tag1 == tag2 = NamedEntity (T.unwords [entity1, entity2]) tag1
mergeToken _ _ = error "Cannot collapse entities with different types"

mergeTokensImpl :: NamedEntity -> [NamedEntity] -> [NamedEntity] -> [NamedEntity]
mergeTokensImpl entity [] resolvedEntities = entity:resolvedEntities
mergeTokensImpl current (next : unresolvedEntities) resolvedEntities
    | _type current == _type next = mergeTokensImpl (mergeToken current next) unresolvedEntities resolvedEntities
    | otherwise                   = mergeTokensImpl next unresolvedEntities (current : resolvedEntities)

mergeTokens :: [NamedEntity] -> [NamedEntity]
mergeTokens [] = []
mergeTokens es = mergeTokensImpl (head es) (tail es) []