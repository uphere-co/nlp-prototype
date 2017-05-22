{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiNamedEntityTagger where

import           Data.Text                             (Text)
import           Data.Vector                           (Vector,toList,fromList,ifoldl')

import           Misc                                  (IRange(..))
import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger                      (NameUIDTable,buildEntityTable,wikiAnnotator)
import           NamedEntity                           (NamedEntity,NamedEntityFrag,NamedEntityClass,parseStr)
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import qualified WikiEntity                    as Wiki
import qualified NamedEntity                   as N
import qualified CoreNLP                       as C

type NEClass = NamedEntityClass

parseStanfordNE :: C.EntityToken -> NamedEntityFrag
parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  parseStr word tag

namedEntityAnnotator:: NameUIDTable -> [NamedEntityFrag] -> [(IRange, Vector Wiki.UID, NEClass)]
namedEntityAnnotator entities frags = map (\(range,uids)->(range,uids, N.Other)) matchedItems
  where
    words = map N._fstr frags
    matchedItems = wikiAnnotator entities words



partitonFrags:: [NamedEntityFrag] -> [(IRange, NEClass)]
partitonFrags frags = ifoldl' f [] (fromList frags)
  where
    incR (IRange beg end) = IRange beg (end+1)
    toRange idx = IRange idx (idx+1)
    tagType = N._ftype
    g idx frag = (toRange idx, tagType frag)
    f [] idx frag = [g idx frag]
    f accum@((range, tag):ss) idx frag | tagType frag == tag = (incR range, tag):ss
                                       | otherwise            = g idx frag : accum

dropNonNE:: [(IRange, NEClass)] -> [(IRange, NEClass)]
dropNonNE = filter (\x-> snd x /= N.Other)

getStanfordNEs :: [NamedEntityFrag] -> [(IRange, NEClass)]
getStanfordNEs = dropNonNE . partitonFrags

buildTagUIDTable :: NEClass -> Vector Wiki.UID -> Vector (Wiki.UID, NEClass)
buildTagUIDTable tag = V.map (\uid -> (uid,tag)) 

data AmbiguousWikiNE = UnresolvedUID NEClass
                     | Resolved Wiki.UID
                     | UnresolvedClass [(Wiki.UID, NEClass)]
                     deriving(Show, Eq)

resolveNEClassImpl :: [(IRange, NEClass)] -> [(IRange, Vector (Wiki.UID, NEClass))] -> [(IRange,AmbiguousWikiNE)] -> [(IRange, AmbiguousWikiNE)]
resolveNEClassImpl [] lhss@((lrange,ltags):ls) accum = (lrange, UnresolvedClass (toList ltags)):accum
resolveNEClassImpl rhss@((rrange,rtag):rs) [] accum  = (rrange, UnresolvedUID rtag) : accum
-- TODO: implement following
resolveNEClassImpl rhss@((rrange,rtag):rs) lhss@((lrange,ltags):ls) accum = undefined 
