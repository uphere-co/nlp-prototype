{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           Data.Text                  (Text)
import           GHC.Generics
import           System.Directory           (createDirectoryIfMissing)
import           System.Process

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
  createDirectoryIfMissing True "build"
  
  callProcess "cmake" ["../rnn++"]
  -- callProcess "make" ["-j20"]
  
  runCommand "./ygpdb_dump /data/groups/uphere/similarity_test/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > ygp.text.ptb"
  runCommand "cat ygp.text.ptb | ./word_count | awk '{print $1}' >> all_words.duplicate"
  runCommand "cat all_words.duplicate | ./word_count | awk '{print $1}' >> all_words"
  runCommand "./word_importance_build dummy"
  runCommand "pigz -dc /opt/wikidata-20170206-all.json.gz | ./wikidata_etl config.ygp.json >wikidata.items"
  runCommand "pigz -c wikidata.items | awk -F $'\t' 'NF==5{print $3}' > wikidata.items.P31"
  runCommand "pigz -c wikidata.items | awk -F $'\t' 'NF==5{print $4}' > wikidata.items.P279"
  runCommand "pigz -c wikidata.items.P31 | tr ' ' '_' | ./word_count > wikidata.items.P31.count"
  runCommand "pigz -c wikidata.items.P279 | tr ' ' '_' | ./word_count > wikidata.items.P279.count"
  runCommand "pigz -c wikidata.items | awk 'BEGIN {FS=\"\t\"};NF==5{print $3 \"\t\" $1}'> items.uid"
  runCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};{print "WIKIDATAITEM_" $1 \"\t\" $NF}' > wikidata.ner_input"
  -- runCommand "java -mx48g edu.stanford.nlp.ie.NERClassifierCombiner -ner.model $CORENLP/classifiers/english.all.3class.distsim.crf.ser.gz,$CORENLP/classifiers/english.conll.4class.distsim.crf.ser.gz,$CORENLP/classifiers/english.muc.7class.distsim.crf.ser.gz -textFile wikidata.ner_input > wikidata.ner"
  
  putStrLn "Pipeline finished!"
