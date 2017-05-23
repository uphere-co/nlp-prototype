{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiNamedEntityTagger where

import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList,ifoldl')

import           Misc                                  (IRange(..))
import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger                      (NameUIDTable,buildEntityTable,wikiAnnotator)
import           NamedEntity                           (NamedEntity,NamedEntityFrag,NamedEntityClass,parseStr)
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import qualified WikiEntity                    as Wiki
import qualified NamedEntity                   as N
import qualified CoreNLP                       as C

parseStanfordNE :: C.EntityToken -> NamedEntityFrag
parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  parseStr word tag

namedEntityAnnotator:: NameUIDTable -> [NamedEntityFrag] -> [(IRange, Vector Wiki.UID, NamedEntityClass)]
namedEntityAnnotator entities frags = map (\(range,uids)->(range,uids, N.Other)) matchedItems
  where
    words = map N._fstr frags
    matchedItems = wikiAnnotator entities words



partitonFrags:: [NamedEntityFrag] -> [(IRange, NamedEntityClass)]
partitonFrags frags = ifoldl' f [] (fromList frags)
  where
    incR (IRange beg end) = IRange beg (end+1)
    toRange idx = IRange idx (idx+1)
    tagType = N._ftype
    g idx frag = (toRange idx, tagType frag)
    f [] idx frag = [g idx frag]
    f accum@((range, tag):ss) idx frag | tagType frag == tag = (incR range, tag):ss
                                       | otherwise            = g idx frag : accum

dropNonNE:: [(IRange, NamedEntityClass)] -> [(IRange, NamedEntityClass)]
dropNonNE = filter (\x-> snd x /= N.Other)

getStanfordNEs :: [NamedEntityFrag] -> [(IRange, NamedEntityClass)]
getStanfordNEs = dropNonNE . partitonFrags