{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EntityLinking where

import qualified Data.Text                  as T
import           NamedEntity                       (NamedEntity)
import qualified NamedEntity                as N


mayRefer :: NamedEntity -> NamedEntity -> Bool
mayRefer src target = N._type src == N._type target && T.isInfixOf (N._str src) (N._str target)
