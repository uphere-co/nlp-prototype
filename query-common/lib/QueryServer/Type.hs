{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QueryServer.Type where

import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.Binary      as Bi
import           Data.Hashable
import           Data.Text              (Text)

data Query = Query { querySentences :: [ Text ] }
  deriving (Show,Eq,Ord)

instance Bi.Binary Query where
  put (Query xs) = Bi.put xs
  get = Query <$> Bi.get

instance Hashable Query where
  hashWithSalt n (Query txts) = hashWithSalt n txts


data RegisteredSentences = RS { rs_sent_uids :: [ Int ] }
  deriving (Show,Eq,Ord)

instance FromJSON RegisteredSentences where
  parseJSON (Object o) = RS <$> o .: "sent_uids"
  parseJSON invalid    = typeMismatch "RegisteredSentences" invalid

instance ToJSON RegisteredSentences where
  toJSON RS {..} = object [ "sent_uids" .= rs_sent_uids ]



data HeartBeat = HB { heartBeat :: Int }

instance Bi.Binary HeartBeat where
  put (HB n) = Bi.put n
  get = HB <$> Bi.get
  
  



   
