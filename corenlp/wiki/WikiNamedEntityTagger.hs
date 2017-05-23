{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiNamedEntityTagger where

import           Data.Text                             (Text)
import           Data.Vector                           (Vector,toList,fromList,ifoldl',foldl')

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


data RelativePosition = LbeforeR | RbeforeL | Coincide | RinL | LinR | LoverlapR | RoverlapL
                      deriving(Show,Eq)

relativePos :: IRange -> IRange -> RelativePosition
relativePos (IRange lbeg lend) (IRange rbeg rend)
  -- Note ordering is crucial for correct pattern matching; do not change it unless unavoidable.
  | lend <= rbeg = LbeforeR
  | rend <= lbeg = RbeforeL
  | lbeg == rbeg && rend == lend = Coincide
  | lbeg <= rbeg && rend <= lend = RinL
  | rbeg <= lbeg && lend <= rend = LinR
  | rbeg < lbeg &&  rend < lend = RoverlapL
  | lbeg < rbeg &&  lend < rend = LoverlapR
  | otherwise = error "Logical bug in nextIRange"

untilNoOverlap :: (a->RelativePosition) -> [a] -> [a]
untilNoOverlap _ [] = []
untilNoOverlap f ranges@(r:_) | LbeforeR == f r = ranges
untilNoOverlap f ranges@(_:rs) = untilNoOverlap f rs

untilOverlapOrNo :: (a->RelativePosition) -> [a] -> [a]
untilOverlapOrNo _ [] = []
untilOverlapOrNo f ranges@(r:rs) = case f r of
  LbeforeR  -> ranges
  LoverlapR -> ranges
  _ -> untilOverlapOrNo f rs

data PreNE = UnresolvedUID NEClass
           | AmbiguousUID [Wiki.UID]
           | Resolved Wiki.UID
           | UnresolvedClass [(Wiki.UID, NEClass)]
           deriving(Show, Eq)
                
resolveNEClass :: NEClass -> Vector (Wiki.UID, NEClass) -> PreNE
resolveNEClass stag xs = g matchedUIDs
  where
    f accum (uid,tag) | tag==stag = uid:accum
                      | otherwise = accum
    matchedUIDs = foldl' f [] xs
    g [uid] = Resolved uid
    g uids  = AmbiguousUID uids

resolveNEsImpl :: [(IRange,PreNE)] -> [(IRange, NEClass)] -> [(IRange, Vector (Wiki.UID, NEClass))] -> [(IRange,PreNE)]
resolveNEsImpl accum [] [] = accum
resolveNEsImpl accum lhss@((lrange,ltag):ls) []  =
  resolveNEsImpl ((lrange, UnresolvedUID ltag) : accum) ls []
resolveNEsImpl accum [] rhss@((rrange,rtags):rs) =
  resolveNEsImpl ((rrange, UnresolvedClass (toList rtags)) : accum) [] rs
resolveNEsImpl accum lhss@((lrange,ltag):ls) rhss@((rrange,rtags):rs) =
  --case mergeDecision (relativePos lrange rrange) of
  case relativePos lrange rrange of
    Coincide  -> resolveNEsImpl ((lrange, resolveNEClass ltag rtags):accum) ls rs
    LinR      -> resolveNEsImpl (keepR:accum) lsIter rs
    RinL      -> resolveNEsImpl (keepL:accum) ls rsIter
    LbeforeR  -> resolveNEsImpl (keepL:accum) ls rhss
    RbeforeL  -> resolveNEsImpl (keepR:accum) lhss rs
    RoverlapL -> resolveNEsImpl (keepR:accum) ls rsIter
    LoverlapR -> resolveNEsImpl (keepL:accum) lsIter rs
  where
    keepL = (lrange, UnresolvedUID ltag)
    keepR = (rrange, UnresolvedClass (toList rtags))
    lsIter = untilNoOverlap (relativePos rrange . fst) ls
    rsIter = untilNoOverlap (relativePos lrange . fst) rs

resolveNEs :: [(IRange, NEClass)] -> [(IRange, Vector (Wiki.UID, NEClass))] -> [(IRange,PreNE)]
resolveNEs = resolveNEsImpl []
