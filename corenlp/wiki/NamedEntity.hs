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

data NamedEntityFrag = NamedEntityFrag { _fstr  :: Text
                                       , _ftype :: NamedEntityClass}
                     | None
                     deriving(Show, Eq)

isSameTYpe :: NamedEntityFrag -> NamedEntityFrag -> Bool
isSameTYpe None _ = False
isSameTYpe _ None = False
isSameTYpe frag1 frag2 = _ftype frag1 == _ftype frag2

parseStr :: Text -> Text -> NamedEntityFrag
parseStr str t | t== "PERSON"      = NamedEntityFrag str Person
               | t== "ORGANIZATION"= NamedEntityFrag str Org
               | t== "LOCATION"    = NamedEntityFrag str Loc
               | t== "TIME"        = NamedEntityFrag str Time
               | t== "DATE"        = NamedEntityFrag str Date
               | t== "O"           = None
parseStr _ _  = error "Unknown named entity class"

mergeToken :: NamedEntityFrag -> NamedEntityFrag -> NamedEntityFrag
mergeToken (NamedEntityFrag entity1 tag1) (NamedEntityFrag entity2 tag2)
  | tag1 == tag2 = NamedEntityFrag (T.unwords [entity1, entity2]) tag1
mergeToken _ _ = error "Cannot collapse entities with different types"

mergeTokensImpl :: NamedEntityFrag -> [NamedEntityFrag]
                -> ([NamedEntityFrag] -> [NamedEntityFrag])
                -> ([NamedEntityFrag] -> [NamedEntityFrag])
mergeTokensImpl None   [] outputBuilder = outputBuilder 
mergeTokensImpl entity [] outputBuilder = outputBuilder <> (entity:)
mergeTokensImpl None (next : unresolvedEntities) outputBuilder = mergeTokensImpl next unresolvedEntities outputBuilder
mergeTokensImpl current (next : unresolvedEntities) outputBuilder
    | isSameTYpe current next = mergeTokensImpl (mergeToken current next) unresolvedEntities outputBuilder
    | otherwise               = mergeTokensImpl next unresolvedEntities (outputBuilder <> (current:) )

mergeTokens :: [NamedEntityFrag] -> [NamedEntityFrag]
mergeTokens []     = []
mergeTokens (e:es) = mergeTokensImpl e es id []
