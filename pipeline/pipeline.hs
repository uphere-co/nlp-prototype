{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           Data.Text                  (Text)
import           GHC.Generics
import           System.Directory           (createDirectoryIfMissing)
import           System.Environment
import           System.Process

data ConfigYGP' = ConfigYGP'
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

data ConfigRSS' = ConfigRSS'
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


data ConfigYGP = ConfigYGP
  { engine_type             :: String
  , corenlp_client_script   :: String
  , word_uids_dump          :: String
  , pos_uids_dump           :: String
  , arclabel_uids_dump      :: String
  , column_uids_dump        :: String
  , country_uids_dump       :: String
  , word_prob_dump          :: String
  , corenlp_dumps           :: String
  , dep_parsed_store        :: String
  , dep_parsed_prefix       :: String
  , wordvec_store           :: String
  , voca_name               :: String
  , w2vmodel_name           :: String
  , w2v_float_t             :: String
  , wikidata_entities       :: String
  , wikidata_uids           :: String
  , wikidata_properties     :: String
  , named_entity_uids       :: String
  } deriving (Show, Generic)

data ConfigRSS = ConfigRSS
  { engine_type             :: String
  , corenlp_client_script   :: String
  , word_uids_dump          :: String
  , pos_uids_dump           :: String
  , arclabel_uids_dump      :: String
  , column_uids_dump        :: String
  , word_prob_dump          :: String
  , row_hashes              :: String
  , corenlp_dumps           :: String
  , dep_parsed_store        :: String
  , dep_parsed_prefix       :: String
  , wordvec_store           :: String
  , voca_name               :: String
  , w2vmodel_name           :: String
  , w2v_float_t             :: String
  , wikidata_entities       :: String
  , wikidata_uids           :: String
  , wikidata_properties     :: String
  , named_entity_uids       :: String
  } deriving (Show, Generic)

defYGP :: ConfigYGP
defYGP = ConfigYGP { engine_type             = "ygp"
                   , corenlp_client_script   = "../rnn++/scripts/corenlp.py"
                   , word_uids_dump          = "/data/groups/uphere/similarity_test/all_words"
                   , pos_uids_dump           = "/data/groups/uphere/similarity_test/poss.uid"
                   , arclabel_uids_dump      = "/data/groups/uphere/similarity_test/dep.uid"
                   , column_uids_dump        = "/data/groups/uphere/similarity_test/column.uid"
                   , country_uids_dump       = "/data/groups/uphere/similarity_test/country.uid"
                   , word_prob_dump          = "/data/groups/uphere/similarity_test/prob.h5"
                   , corenlp_dumps           = "ygp.corenlp" -- "/data/groups/uphere/similarity_test/ygp.corenlp"
                   , dep_parsed_store        = "ygp.h5" -- "/data/groups/uphere/similarity_test/ygp.h5"
                   , dep_parsed_prefix       = "ygp"
                   , wordvec_store           = "/data/groups/uphere/similarity_test/news.h5"
                   , voca_name               = "news.en.uids"
                   , w2vmodel_name           = "news.en.vecs"
                   , w2v_float_t             = "float32"
                   , wikidata_entities       = "/home/jihuni/word2vec/rss/wikidata.all_entities"
                   , wikidata_uids           = "/home/jihuni/word2vec/rss/wikidata.uid"
                   , wikidata_properties     = "/home/jihuni/word2vec/rss/wikidata.properties"
                   , named_entity_uids       = "/home/jihuni/word2vec/rss/wikidata.uid.ne"
                   }

defRSS :: ConfigRSS
defRSS = ConfigRSS
  { engine_type                              = "rss"
  , corenlp_client_script                    = "../rnn++/scripts/corenlp.py"
  , word_uids_dump                           = "/data/groups/uphere/engine.rss/all_words"
  , pos_uids_dump                            = "/data/groups/uphere/engine.rss/poss.uid"
  , arclabel_uids_dump                       = "/data/groups/uphere/engine.rss/dep.uid"
  , column_uids_dump                         = "/data/groups/uphere/engine.rss/column.uid"
  , word_prob_dump                           = "/data/groups/uphere/engine.rss/prob.h5"
  , row_hashes                               = "/data/groups/uphere/engine.rss/article.hashes"
  , corenlp_dumps                            = "/data/groups/uphere/engine.rss/article.corenlp"
  , dep_parsed_store                         = "/data/groups/uphere/engine.rss/nyt.h5"
  , dep_parsed_prefix                        = "nyt"
  , wordvec_store                            = "/data/groups/uphere/engine.rss/news.h5"
  , voca_name                                = "news.en.uids"
  , w2vmodel_name                            = "news.en.vecs"
  , w2v_float_t                              = "float32"
  , wikidata_entities                        = "/data/groups/uphere/engine.rss/wikidata.all_entities"
  , wikidata_uids                            = "/data/groups/uphere/engine.rss/wikidata.uid"
  , wikidata_properties                      = "/data/groups/uphere/engine.rss/wikidata.properties"
  , named_entity_uids                        = "/data/groups/uphere/engine.rss/wikidata.uid.ne"
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
  
  callProcess "cmake" ["../../rnn++"]
  callProcess "make" ["-j20"]  
  callCommand "./ygpdb_dump /data/groups/uphere/similarity_test/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > ygp.text.ptb"
  callCommand "cat ygp.text.ptb | ./word_count | awk '{print $1}' >> all_words.duplicate"
  callCommand "cat all_words.duplicate | ./word_count | awk '{print $1}' >> all_words"
  callCommand "pigz -dc /opt/wikidata-20170206-all.json.gz | ./wikidata_etl config.ygp.json >wikidata.items"
  callCommand "cat wikidata.items | awk -F $'\t' 'NF==5{print $3}' > wikidata.items.P31"
  callCommand "cat wikidata.items | awk -F $'\t' 'NF==5{print $4}' > wikidata.items.P279"
  callCommand "cat wikidata.items.P31 | tr ' ' '_' | ./word_count > wikidata.items.P31.count"
  callCommand "cat wikidata.items.P279 | tr ' ' '_' | ./word_count > wikidata.items.P279.count"
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};NF==5{print $3 \"\t\" $1}'> items.uid"
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};{print \"WIKIDATAITEM_\" $1 \"\t\" $NF}' > wikidata.ner_input"

  let corenlpEnv = "/data/groups/uphere/parsers/corenlp"
  -- callCommand $ "java -mx48g edu.stanford.nlp.ie.NERClassifierCombiner -ner.model "++ corenlpEnv ++"/classifiers/english.all.3class.distsim.crf.ser.gz,"++ corenlpEnv ++"/classifiers/english.conll.4class.distsim.crf.ser.gz,"++ corenlpEnv ++"/classifiers/english.muc.7class.distsim.crf.ser.gz -textFile wikidata.ner_input > wikidata.ner" -- very time-consuming

  callCommand "ghc -o count ../../corenlp/wiki/count.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o corenlp_ner ../../corenlp/wiki/corenlp_ner.hs ../../corenlp/wiki/corenlp.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o ne_by_property ../../corenlp/wiki/ne_by_property.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o wikidata_ner ../../corenlp/wiki/wikidata_ner.hs ../../corenlp/wiki/wikidata.hs"

  callCommand "./corenlp_ner  > wikidata.is_sfne"
  callCommand "cat wikidata.is_sfne | awk 'BEGIN {FS=\"\t\"};{print $1 \"\t\" $NF}' > wikidata.uid.is_sfne"
  callCommand "./ne_by_property > wikidata.p31.is_ne"
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};NF==5{print $1 \"\t\" $3  \"\t\" $NF}' > wikidata.names"
  callCommand "./wikidata_ner > wikidata.nes"
  -- callCommand "./count > items.by_p31"

  callCommand "cat wikidata.nes | awk -F '\t' '$2==\"True\"{print $1}' > wikidata.uid.ne"
  callCommand "cat wikidata.items | awk -F '\t' 'NF==5{print $1}' > wikidata.uid"
  callCommand "cat wikidata.items | awk -F '\t' '{print $1 \"\t\" $NF}' > wikidata.all_entities"
  callCommand "cat wikidata.items | awk -F '\t' 'NF==5{print $1 \"\t\" $3}' > wikidata.properties"

  -- callCommand "find /opt/YGP.text/ -type f | cat -n | xargs -P 20 -i'{}' python ../../rss_crawler/parse_article.py YGP {} /opt/RSS.text/"
  createDirectoryIfMissing True "YGP.json"
  -- callCommand "find /opt/YGP.text -type f | xargs -P20 -I {} python ../../rnn++/scripts/corenlp.py {} YGP.json/" -- time-consuming
  callCommand "find YGP.json/  -name '*.*.*.*' > ygp.corenlp"
  let minorVersion = ("0" :: String)
  callCommand $ "make -j20 && time ./ygpdb_etl config.ygp.json ygp.corenlp " ++ minorVersion


  -- callCommand "./word_importance_build"
  
  -- callCommand "find YGP.json/ -type f > ygp_jsons" -- for RSS
  -- callCommand "./ygp_dump config.ygp.json ygp_jsons 1" -- for RSS

  putStrLn "Pipeline finished!"
