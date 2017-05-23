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

untilNoOverlap :: IRange -> [IRange] -> [IRange]
untilNoOverlap _ [] = []
untilNoOverlap ref ranges@(r:_) | LbeforeR == relativePos ref r = ranges
untilNoOverlap ref ranges@(_:rs) = untilNoOverlap ref rs

untilOverlapOrNo :: IRange -> [IRange] -> [IRange]
untilOverlapOrNo _ [] = []
untilOverlapOrNo ref ranges@(r:rs) = case relativePos ref r of
  LbeforeR  -> ranges
  LoverlapR -> ranges
  _ -> untilOverlapOrNo ref rs

data PreNE = UnresolvedUID NEClass
           | AmbiguousUID [Wiki.UID]
           | Resolved Wiki.UID
           | UnresolvedClass [(Wiki.UID, NEClass)]
           deriving(Show, Eq)
data NextItem a = Left a
                | Right a
                | LeftRight a
                deriving(Show)
                
resolveNEClass :: NEClass -> Vector (Wiki.UID, NEClass) -> PreNE
resolveNEClass stag xs = g matchedUIDs
  where
    f accum (uid,tag) | tag==stag = uid:accum
                      | otherwise = accum
    matchedUIDs = foldl' f [] xs
    g [uid] = Resolved uid
    g uids  = AmbiguousUID uids

data MergeDecision = KeepL | KeepR | MergeLR

mergeDecision :: RelativePosition -> MergeDecision
mergeDecision pos = case pos of
  Coincide  -> MergeLR
  LinR      -> KeepR
  RinL      -> KeepL
  LbeforeR  -> KeepL
  RbeforeL  -> KeepR
  RoverlapL -> KeepR
  LoverlapR -> KeepL

resolveNE :: (IRange, NEClass) -> (IRange, Vector (Wiki.UID, NEClass)) -> (IRange,PreNE)
resolveNE (lrange, ltag) (rrange, rtags) = 
  case mergeDecision (relativePos lrange rrange) of
    KeepL   -> (lrange, UnresolvedUID ltag)
    KeepR   -> (rrange, UnresolvedClass (toList rtags))
    MergeLR -> (lrange, resolveNEClass ltag rtags)

resolveNEsImpl :: [(IRange,PreNE)] -> [(IRange, NEClass)] -> [(IRange, Vector (Wiki.UID, NEClass))] -> [(IRange,PreNE)]
resolveNEsImpl accum [] [] = accum
resolveNEsImpl accum lhss@((lrange,ltag):ls) []  = resolveNEsImpl ((lrange, UnresolvedUID ltag) : accum) ls []
resolveNEsImpl accum [] rhss@((rrange,rtags):rs) = resolveNEsImpl ((rrange, UnresolvedClass (toList rtags)) : accum) [] rs
-- TODO: implement following
resolveNEsImpl lhss@((lrange,ltags):ls) rhss@((rrange,rtag):rs) accum = undefined 

resolveNEs :: [(IRange, NEClass)] -> [(IRange, Vector (Wiki.UID, NEClass))] -> [(IRange,PreNE)]
resolveNEs = resolveNEsImpl []
