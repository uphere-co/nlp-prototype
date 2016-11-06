{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QueryServer.Type where

import           Control.Applicative    ((<|>))
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.Binary      as Bi
import           Data.Hashable
import           Data.Text              (Text)

data Query = QueryText { query_text :: Text }
  deriving (Show,Eq,Ord)

instance Bi.Binary Query where
  put (QueryText txt) = Bi.put txt
  get = QueryText <$> Bi.get

instance Hashable Query where
  hashWithSalt n (QueryText txt) = hashWithSalt n txt


data RegisteredSentences = RS { rs_sent_uids :: [ Int ]
                              , rs_max_clip_len :: Maybe Int
                              }
  deriving (Show,Eq,Ord)

instance FromJSON RegisteredSentences where
  parseJSON (Object o) = do suids <- o .: "sent_uids"
                            ((o .: "max_clip_len" >>= \n -> return (RS suids (Just n)))
                             <|> return (RS suids Nothing))  
  parseJSON invalid    = typeMismatch "RegisteredSentences" invalid

instance ToJSON RegisteredSentences where
  toJSON RS {..} = object $ [ "sent_uids" .= rs_sent_uids ] ++
                            maybe [] (\x -> ["max_clip_len" .= x]) rs_max_clip_len



data HeartBeat = HB { heartBeat :: Int }

instance Bi.Binary HeartBeat where
  put (HB n) = Bi.put n
  get = HB <$> Bi.get
  
  



   
