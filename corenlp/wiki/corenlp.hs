{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CoreNLP where

import           Data.Text                         (Text)

newtype WordToken = WordToken   { unWord :: Text}
                  deriving (Show, Eq, Ord)
newtype NETag     = NETag { unNETag :: Text}
                  deriving (Show, Eq, Ord)

data EntityToken = EntityToken { word :: WordToken
                               , tag  :: NETag }
