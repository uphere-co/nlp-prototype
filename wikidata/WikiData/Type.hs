{-# LANGUAGE OverloadedStrings #-}

module WikiData.Type where

import           Control.Applicative              ((<|>))
import           Data.Aeson
import qualified Data.Aeson.Types           as AT
import qualified Data.HashMap.Strict        as HM
import           Data.Text                        (Text)


data LangValue = LV { lv_language :: Text
                    , lv_value :: Text
                    } deriving (Show, Eq)

instance FromJSON LangValue where
  parseJSON (Object o) = LV <$> (o .: "language")
                            <*> (o .: "value")
  parseJSON invalid = AT.typeMismatch "LangValue" invalid

data Snak = Snak { snak_snaktype :: Text
                 , snak_property :: Text
                 , snak_datatype :: Maybe Text
                 , snak_datavalue :: Maybe Value
                 } deriving (Show, Eq)

instance FromJSON Snak where
  parseJSON (Object o) = Snak <$> (o .: "snaktype")
                              <*> (o .: "property")
                              <*> ((Just <$> (o .: "datatype")) <|> return Nothing)
                              <*> ((Just <$> (o .: "datavalue")) <|> return Nothing)
  parseJSON invalid = AT.typeMismatch "Snak" invalid

data Reference = Ref { ref_hash :: Text
                     , ref_snaks :: HM.HashMap Text [Snak]
                     } deriving (Show, Eq)

instance FromJSON Reference where
  parseJSON (Object o) = Ref <$> (o .: "hash")
                              <*> (o .: "snaks")
  parseJSON invalid = AT.typeMismatch "Reference" invalid

data Claim = Claim { claim_id :: Text
                   , claim_mainsnak :: Snak
                   , claim_type :: Text
                   , claim_rank :: Text
                   , claim_qualifiers :: Maybe [Snak]
                   , claim_references :: Maybe [Reference]
                   } deriving (Show, Eq)

instance FromJSON Claim where
  parseJSON (Object o) = Claim <$> (o .: "id")
                               <*> (o .: "mainsnak")
                               <*> (o .: "type")
                               <*> (o .: "rank")
                               <*> ((Just <$> (o .: "qualifiers")) <|> return Nothing)
                               <*> ((Just <$> (o .: "references")) <|> return Nothing)
  parseJSON invalid = AT.typeMismatch "Claim" invalid

data TopLevel = TopLevel { toplevel_id :: Text
                         , toplevel_type :: Text
                         , toplevel_labels :: HM.HashMap Text LangValue
                         , toplevel_descriptions :: HM.HashMap Text LangValue
                         , toplevel_aliases :: HM.HashMap Text [LangValue]
                         , toplevel_claims :: HM.HashMap Text [Claim]
                         , toplevel_sitelinks :: Value
                         , toplevel_lastrevid :: Maybe Int
                         , toplevel_modified :: Maybe Text
                         } deriving (Show,Eq)

instance FromJSON TopLevel where
  parseJSON (Object o) =
    TopLevel <$> (o .: "id")
             <*> (o .: "type")
             <*> (o .: "labels") 
             <*> (o .: "descriptions") 
             <*> (o .: "aliases") 
             <*> (o .: "claims")
             <*> (o .: "sitelinks")
             <*> ((Just <$> (o .: "lastrevid")) <|> return Nothing )
             <*> ((Just <$> (o .: "modified")) <|> return Nothing)
  parseJSON invalid = AT.typeMismatch "TopLevel" invalid
   
