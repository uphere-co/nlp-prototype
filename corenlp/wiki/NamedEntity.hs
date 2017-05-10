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
                     deriving(Show, Eq)


isSameTYpe :: NamedEntityFrag -> NamedEntityFrag -> Bool
isSameTYpe frag1 frag2 = _ftype frag1 == _ftype frag2

parseStr :: Text -> Text -> NamedEntityFrag
parseStr str t | t== "PERSON"      = NamedEntityFrag str Person
               | t== "ORGANIZATION"= NamedEntityFrag str Org
               | t== "LOCATION"    = NamedEntityFrag str Loc
               | t== "TIME"        = NamedEntityFrag str Time
               | t== "DATE"        = NamedEntityFrag str Date
               | t== "O"           = NamedEntityFrag str Other
parseStr _ _  = error "Unknown named entity class"

mergeToken :: [NamedEntityFrag] -> [NamedEntity]
mergeToken (NamedEntityFrag str tag : es) | tag /= Other = [NamedEntity ss tag] where ss = T.unwords (str : map _fstr es)
mergeToken _ = []

partitionTokensImpl :: [NamedEntityFrag]-> [NamedEntityFrag] -> [[NamedEntityFrag]] -> [[NamedEntityFrag]]
partitionTokensImpl entities [] outputs = outputs
partitionTokensImpl current (next : unresolvedEntities) outputs
    | isSameTYpe (head current) next = partitionTokensImpl (current ++ [next]) unresolvedEntities outputs
    | otherwise                      = partitionTokensImpl [next] unresolvedEntities (outputs ++ [current])

partitionTokens :: [NamedEntityFrag] -> [[NamedEntityFrag]]
partitionTokens [] = [[]]
partitionTokens (e:es) = partitionTokensImpl [e] es []

mergeTokens :: [NamedEntityFrag] -> [NamedEntity]
mergeTokens es = concatMap mergeToken (partitionTokens es)

