{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wikidata where

import           Data.Text                         (Text)

newtype Name = Name { unName :: Text}
             deriving (Show, Eq, Ord)
newtype UID  = UID { unUID :: Text}
             deriving (Show, Eq, Ord)
newtype P31  = P31 { unP31 :: Text}
             deriving (Show, Eq, Ord)
