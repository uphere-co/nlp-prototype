{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EntityLinking where

import           Data.List                             (inits,foldl')
import           Data.Vector                           (Vector)
import qualified Data.Text                  as T
import           Data.Text                             (Text)

import           Misc                                  (IRange(..),RelativePosition(..),relativePos,isContain,subVector) 
import           NamedEntity                           (NamedEntity, OrderedNamedEntity)
import           WikiNamedEntityTagger                 (PreNE(..))
import qualified NamedEntity                as N

mayRefer :: NamedEntity -> NamedEntity -> Bool
mayRefer src target = (N._type src == N._type target) && T.isInfixOf (N._str src) (N._str target)

canRefer :: OrderedNamedEntity -> OrderedNamedEntity -> Bool
canRefer src target = (N._order src > N._order target) && mayRefer (N._entity src) (N._entity target)


newtype EntityMentionUID = EntityMentionUID { _uid :: Int}

instance Show EntityMentionUID where
  show (EntityMentionUID _uid) = "EMuid " ++ show _uid

data UIDCite uid info = Cite uid uid info
                      | Self uid info
                      deriving(Show, Eq)

getUID (Self uid _)   = uid
getUID (Cite uid _ _) = uid
getInfo (Self _ info) = info
getInfo (Cite _ _ info) = info

-- w : type of word token
type EMuid = EntityMentionUID
type EMInfo w = (IRange, Vector w, PreNE)
type EntityMention w = UIDCite EMuid (EMInfo w)

buildEntityMentions :: Vector w -> [(IRange, PreNE)] -> [EntityMention w]
buildEntityMentions text wikiNEs = zipWith Self uids mentions
  where
    uids     = map EntityMentionUID [1..]
    mentions = map (\(range,e) -> (range, subVector range text, e)) wikiNEs


isEntityLink :: Eq a => EMInfo a -> EMInfo a -> Maybe (EMInfo a)
isEntityLink target@(trange, twords, ttag) src@(srange, swords, stag) =
  f (relativePos trange srange) (isContain swords twords) ttag stag
  where 
    f pos textMatch (Resolved (uid,ttag)) (UnresolvedUID stag) | pos==LbeforeR && textMatch && ttag==stag =
      Just (srange,swords, Resolved (uid,ttag))
    f _ _ _ _ = Nothing

entityLinking :: Eq a => [EntityMention a] -> EntityMention a -> EntityMention a
entityLinking targets src = foldr f src targets
  where
    f target (Self idx src)       =
      case isEntityLink (getInfo target) src of
        Nothing   -> Self idx src
        Just info -> Cite idx (getUID target) info
    f _      (Cite uid ref info) = Cite uid ref info

entityLinkings :: Eq a => [EntityMention a] -> [EntityMention a]
entityLinkings srcs = reverse (foldl' f [] srcs)
  where
    f targets src = entityLinking targets src : targets


