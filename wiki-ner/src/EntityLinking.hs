{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EntityLinking where

import           Data.List                             (inits,foldl')
import           Data.Vector                           (Vector)
import qualified Data.Text                  as T

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
  show (EntityMentionUID _uid) = "EMIdx " ++ show _uid

type EMuid = EntityMentionUID


data EntityMention idx a = Mention a
                         | Self idx a
                         deriving(Show, Eq)

unMention (Mention a) = a
unMention (Self idx a) = a

type EMInfo a = (IRange, Vector a, PreNE)

-- w : type of word token
buildEntityMentions :: Vector w -> [(IRange, PreNE)] -> [EntityMention EMuid (EMInfo w)]
buildEntityMentions text wikiNEs = output
  where
    output = map (\(range,e) -> Self (EntityMentionUID 0) (range, subVector range text, e)) wikiNEs

entityLink :: Eq a => EMInfo a -> EMInfo a -> EntityMention EMuid (EMInfo a)
entityLink target@(trange, twords, ttag) src@(srange, swords, stag) =
  f (relativePos trange srange) (isContain swords twords) ttag stag
  where 
    f pos textMatch (Resolved (uid,ttag)) (UnresolvedUID stag) | pos==LbeforeR && textMatch && ttag==stag =
      Mention (srange,swords,Resolved (uid,ttag))
    f _ _ _ _ = Self (EntityMentionUID 0) src

entityLinking :: Eq a => [EMInfo a] -> EMInfo a -> EntityMention EMuid (EMInfo a)
entityLinking targets src = foldr f (Self (EntityMentionUID 0) src) targets
  where
    f target (Self idx src)       = entityLink target src
    f _      (Mention target) = Mention target

entityLinkings :: Eq a => [EMInfo a] -> [EntityMention EMuid (EMInfo a)]
entityLinkings srcs = reverse (foldl' f [] srcs)
  where
    f targets src = entityLinking (map unMention targets) src : targets


