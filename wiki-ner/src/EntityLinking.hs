{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EntityLinking where

import           Data.List                             (inits,foldl')
import           Data.Vector                           (Vector)
import qualified Data.Text                  as T

import           Misc                                  (IRange(..),RelativePosition(..),relativePos,isContain) 
import           NamedEntity                           (NamedEntity, OrderedNamedEntity)
import           WikiNamedEntityTagger                 (PreNE(..))
import qualified NamedEntity                as N

mayRefer :: NamedEntity -> NamedEntity -> Bool
mayRefer src target = (N._type src == N._type target) && T.isInfixOf (N._str src) (N._str target)

canRefer :: OrderedNamedEntity -> OrderedNamedEntity -> Bool
canRefer src target = (N._order src > N._order target) && mayRefer (N._entity src) (N._entity target)

data EntityMention a = Mention a
                     | Self a
                     deriving(Show, Eq)

unMention (Mention a) = a
unMention (Self a) = a

type EMInfo a = (IRange, Vector a, PreNE)
entityLink :: Eq a => EMInfo a -> EMInfo a -> EntityMention (EMInfo a)
entityLink target src = x
  where 
    (trange, twords, ttag) = target
    (srange, swords, stag) = src
    f pos textMatch (Resolved (uid,ttag)) (UnresolvedUID stag) | pos==LbeforeR && textMatch && ttag==stag =
      Mention (srange,swords,Resolved (uid,ttag))
    f _ _ _ _ = Self src
    x = f (relativePos trange srange) (isContain swords twords) ttag stag

entityLinking :: Eq a => [EMInfo a] -> EMInfo a -> EntityMention (EMInfo a)
entityLinking targets src = foldr f (Self src) targets
  where
    f target (Self src)  = entityLink target src
    f _      (Mention target) = Mention target

entityLinkings :: Eq a => [EMInfo a] -> [EntityMention (EMInfo a)]
entityLinkings srcs = reverse (foldl' f [] srcs)
  where
    f targets src = entityLinking (map unMention targets) src : targets


