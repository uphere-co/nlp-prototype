{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           Data.Text                  (Text)
import           GHC.Generics

data ConfigYGP = ConfigYGP
  { _engine_type             :: String
  , _corenlp_client_script   :: String
  , _word_uids_dump          :: String
  , _pos_uids_dump           :: String
  , _arclabel_uids_dump      :: String
  , _column_uids_dump        :: String
  , _country_uids_dump       :: String
  , _word_prob_dump          :: String
  , _corenlp_dumps           :: String
  , _dep_parsed_store        :: String
  , _dep_parsed_prefix       :: String
  , _wordvec_store           :: String
  , _voca_name               :: String
  , _w2vmodel_name           :: String
  , _w2v_float_t             :: String
  , _wikidata_entities       :: String
  , _wikidata_uids           :: String
  , _wikidata_properties     :: String
  , _named_entity_uids       :: String
  } deriving (Show, Generic)

data ConfigRSS = ConfigRSS
  { _engine_type             :: String
  , _corenlp_client_script   :: String
  , _word_uids_dump          :: String
  , _pos_uids_dump           :: String
  , _arclabel_uids_dump      :: String
  , _column_uids_dump        :: String
  , _word_prob_dump          :: String
  , _row_hashes              :: String
  , _corenlp_dumps           :: String
  , _dep_parsed_store        :: String
  , _dep_parsed_prefix       :: String
  , _wordvec_store           :: String
  , _voca_name               :: String
  , _w2vmodel_name           :: String
  , _w2v_float_t             :: String
  , _wikidata_entities       :: String
  , _wikidata_uids           :: String
  , _wikidata_properties     :: String
  , _named_entity_uids       :: String
  } deriving (Show, Generic)



defYGP :: ConfigYGP
defYGP = ConfigYGP { _engine_type             = "ygp"
                   , _corenlp_client_script   = "../rnn++/scripts/corenlp.py"
                   , _word_uids_dump          = "/data/groups/uphere/similarity_test/all_words"
                   , _pos_uids_dump           = "/data/groups/uphere/similarity_test/poss.uid"
                   , _arclabel_uids_dump      = "/data/groups/uphere/similarity_test/dep.uid"
                   , _column_uids_dump        = "/data/groups/uphere/similarity_test/column.uid"
                   , _country_uids_dump       = "/data/groups/uphere/similarity_test/country.uid"
                   , _word_prob_dump          = "/data/groups/uphere/similarity_test/prob.h5"
                   , _corenlp_dumps           = "/data/groups/uphere/similarity_test/ygp.corenlp"
                   , _dep_parsed_store        = "/data/groups/uphere/similarity_test/ygp.h5"
                   , _dep_parsed_prefix       = "ygp"
                   , _wordvec_store           = "/data/groups/uphere/similarity_test/news.h5"
                   , _voca_name               = "news.en.uids"
                   , _w2vmodel_name           = "news.en.vecs"
                   , _w2v_float_t             = "float32"
                   , _wikidata_entities       = "/home/jihuni/word2vec/rss/wikidata.all_entities"
                   , _wikidata_uids           = "/home/jihuni/word2vec/rss/wikidata.uid"
                   , _wikidata_properties     = "/home/jihuni/word2vec/rss/wikidata.properties"
                   , _named_entity_uids       = "/home/jihuni/word2vec/rss/wikidata.uid.ne"
                   }

defRSS :: ConfigRSS
defRSS = ConfigRSS
  { _engine_type                              = "rss"
  , _corenlp_client_script                    = "../rnn++/scripts/corenlp.py"
  , _word_uids_dump                           = "/data/groups/uphere/engine.rss/all_words"
  , _pos_uids_dump                            = "/data/groups/uphere/engine.rss/poss.uid"
  , _arclabel_uids_dump                       = "/data/groups/uphere/engine.rss/dep.uid"
  , _column_uids_dump                         = "/data/groups/uphere/engine.rss/column.uid"
  , _word_prob_dump                           = "/data/groups/uphere/engine.rss/prob.h5"
  , _row_hashes                               = "/data/groups/uphere/engine.rss/article.hashes"
  , _corenlp_dumps                            = "/data/groups/uphere/engine.rss/article.corenlp"
  , _dep_parsed_store                         = "/data/groups/uphere/engine.rss/nyt.h5"
  , _dep_parsed_prefix                        = "nyt"
  , _wordvec_store                            = "/data/groups/uphere/engine.rss/news.h5"
  , _voca_name                                = "news.en.uids"
  , _w2vmodel_name                            = "news.en.vecs"
  , _w2v_float_t                              = "float32"
  , _wikidata_entities                        = "/data/groups/uphere/engine.rss/wikidata.all_entities"
  , _wikidata_uids                            = "/data/groups/uphere/engine.rss/wikidata.uid"
  , _wikidata_properties                      = "/data/groups/uphere/engine.rss/wikidata.properties"
  , _named_entity_uids                        = "/data/groups/uphere/engine.rss/wikidata.uid.ne"
  }



  
-- Not customizable.
instance ToJSON ConfigYGP where
    toJSON = genericToJSON defaultOptions

instance FromJSON ConfigYGP where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ConfigRSS where
    toJSON = genericToJSON defaultOptions

instance FromJSON ConfigRSS where
    parseJSON = genericParseJSON defaultOptions
--

main = do
  BL.writeFile "config.ygp.json" (encode defYGP)
  BL.writeFile "config.rss.json" (encode defRSS)
  putStrLn "pipeline"
