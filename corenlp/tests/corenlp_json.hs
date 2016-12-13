{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative               ((<|>))
import           Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T.E
import qualified Data.Text.IO               as T.IO
import           GHC.Generics

newtype ArcLabel = ArcLabel Text
             deriving (Eq, Show, ToJSON, FromJSON)
newtype Dep = Dep Text
             deriving (Eq, Show, ToJSON, FromJSON)
newtype DepPos = DepPos Int
             deriving (Eq, Show, ToJSON, FromJSON)
newtype Gov = Gov Text
             deriving (Eq, Show, ToJSON, FromJSON)
newtype GovPos = GovPos Int
             deriving (Eq, Show, ToJSON, FromJSON)

data DepToken = MkDepToken  { arclabel :: ArcLabel
                            , dep      :: Dep
                            , dep_pos  :: DepPos
                            , gov      :: Gov
			    , gov_pos  :: GovPos }
            deriving (Eq, Show)
{-
data DepToken = DepToken  { dep :: ArcLabel
                            , dependentGloss      :: Dep
                            , dependent  :: DepPos
                            , governorGloss      :: Gov
			    , governor  :: GovPos }
--            deriving (Eq, Show, FromJSON, ToJSON)
            deriving (Eq, Show, Generic)
instance FromJSON DepToken
instance ToJSON DepToken
-}

newtype POS = POS Text
             deriving (Eq, Show, ToJSON, FromJSON)
newtype Offset = Offset Int
             deriving (Eq, Show, ToJSON, FromJSON)
newtype WordOriginal = WordOriginal Text
             deriving (Eq, Show, ToJSON, FromJSON)

data WordToken = MkWordToken  { word_pos :: DepPos
                              , word     :: Dep
                              , pos      :: POS
                              , offset_front :: Offset
                              , offset_back  :: Offset
                              , word_orig    :: WordOriginal }
            deriving (Eq, Show)

data SentenceDep = MkSentenceDep { deps  :: [DepToken]
                                 , words :: [WordToken] }
            deriving (Eq, Show)
data ParseJSONChunk = MkParseJSONChunk { sents :: [SentenceDep] }
            deriving (Eq, Show)

instance ToJSON DepToken where
  toJSON (MkDepToken arc dep dep_pos gov gov_pos)
    = object [ "dep" .= arc,  "dependent" .= dep_pos,  "dependentGloss" .= dep,  "governor" .= gov_pos, "governorGloss" .= gov ] 

instance FromJSON DepToken where
  parseJSON (Object v) =
    (MkDepToken <$> v .: "dep" <*> v .: "dependentGloss"<*> v .: "dependent"<*> v .: "governorGloss"<*> v .: "governor")
  parseJSON wat = Data.Aeson.Types.typeMismatch "DepToken" wat 


instance ToJSON WordToken where
  toJSON (MkWordToken word_pos word pos offset_front offset_back word_orig)
    = object [ "index" .= word_pos,  "word" .= word,  "pos" .= pos,  
               "characterOffsetEnd" .= offset_front, "characterOffsetBegin" .= offset_back,
               "originalText" .= word_orig ] 

instance FromJSON WordToken where
  parseJSON (Object v) =
    (MkWordToken <$> v .: "index" <*> v .: "word"<*> v .: "pos" 
                 <*> v .: "characterOffsetEnd"<*> v .: "characterOffsetBegin"
                 <*> v .: "originalText")
  parseJSON wat = Data.Aeson.Types.typeMismatch "WordToken" wat 



instance ToJSON SentenceDep where
  toJSON (MkSentenceDep deps words)
    = object [ "basicDependencies" .= deps,  "tokens" .= words ] 

instance FromJSON SentenceDep where
  parseJSON (Object v) =
    (MkSentenceDep <$> v .: "basicDependencies" <*> v .: "tokens" )
  parseJSON wat = Data.Aeson.Types.typeMismatch "SentenceDep" wat 

instance ToJSON ParseJSONChunk where
  toJSON (MkParseJSONChunk sents)
    = object [ "sentences" .= sents ] 

instance FromJSON ParseJSONChunk where
  parseJSON (Object v) =
    (MkParseJSONChunk <$> v .: "sentences" )
  parseJSON wat = Data.Aeson.Types.typeMismatch "ParseJSONChunk" wat 


tryDump :: Either String ParseJSONChunk -> BL.ByteString
tryDump (Right json) = encode json
tryDump (Left _ )    = "null"


assert :: Bool -> String
assert True = "Test passed"

--{-

main :: IO ()
main = do
  jsonstr <- BL.readFile "data/dep_token.json"
  print (eitherDecode jsonstr :: Either String DepToken)
  
  jsonstr <- BL.readFile "data/word_token.json"
  print (eitherDecode jsonstr :: Either String WordToken)

  jsonstr <- BL.readFile "data/sent.json"
  let
    ej = eitherDecode jsonstr :: Either String ParseJSONChunk
    jsondump = tryDump ej
    ejd = eitherDecode jsondump :: Either String ParseJSONChunk
  print.assert$ ej==ejd

--}
