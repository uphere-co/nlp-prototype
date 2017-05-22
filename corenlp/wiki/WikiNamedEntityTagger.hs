{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiNamedEntityTagger where

import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList,ifoldl')

import           Misc                                  (IRange(..))
import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger                      (EntityTable,buildEntityTable,wikiAnnotator)
import           NamedEntity                           (NamedEntity,NamedEntityFrag,NamedEntityClass,parseStr)
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import qualified WikiEntity                    as Wiki
import qualified NamedEntity                   as N
import qualified CoreNLP                       as C

parseStanfordNE :: C.EntityToken -> NamedEntityFrag
parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  parseStr word tag

namedEntityAnnotator:: EntityTable -> [NamedEntityFrag] -> [(IRange, Vector Wiki.UID, NamedEntityClass)]
namedEntityAnnotator entities frags = map (\(range,uids)->(range,uids, N.Other)) matchedItems
  where
    words = map N._fstr frags
    matchedItems = wikiAnnotator entities words


partitonFrags:: [NamedEntityFrag] -> [(IRange, NamedEntityClass)]
partitonFrags frags = ifoldl' f [] (fromList frags)
  where
    g idx frag = (IRange idx (idx+1), N._ftype frag)
    f [] idx frag = [g idx frag]
    f accum@((IRange beg end, tag):ss) idx frag | N._ftype frag == tag = (IRange beg (end+1), tag):ss
                                                | otherwise            = g idx frag : accum

dropNonNE:: [(IRange, NamedEntityClass)] -> [(IRange, NamedEntityClass)]
dropNonNE = filter (\(_, tag)-> tag /= N.Other)

getStanfordNEs :: [NamedEntityFrag] -> [(IRange, NamedEntityClass)]
getStanfordNEs frags = dropNonNE (partitonFrags frags)