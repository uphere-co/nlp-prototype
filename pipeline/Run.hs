module Run where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           System.Directory           (createDirectoryIfMissing)
import           System.Environment
import           System.Process
--
import      Type

mkRnnApp :: IO ()
mkRnnApp = do
  -- Build apps in rnn++.
  callProcess "cmake" ["../../rnn++"]
  callProcess "make" ["-j20"]
  putStrLn "rnn++ app build completed"

mkCoreNLPApp :: IO ()
mkCoreNLPApp = do
  -- Build apps in corenlp.
  callCommand "ghc -o count ../../corenlp/wiki/count.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o corenlp_ner ../../corenlp/wiki/corenlp_ner.hs ../../corenlp/wiki/corenlp.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o ne_by_property ../../corenlp/wiki/ne_by_property.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o wikidata_ner ../../corenlp/wiki/wikidata_ner.hs ../../corenlp/wiki/wikidata.hs"
  putStrLn "corenlp app build completed"

-- Write JSON config files.
writeJSON :: EngineType -> IO ()
writeJSON typ = do
  case typ of
    YGP -> getDefYGP >>= (\x -> BL.writeFile "config.ygp.json" (encode x))
    RSS -> getDefRSS >>= (\x -> BL.writeFile "config.rss.json" (encode x))


runYGP :: IO ()
runYGP = do  
  -- Dump the YGP DB, and produce all_words
  callCommand "./ygpdb_dump /data/groups/uphere/similarity_test/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > ygp.text.ptb"
  callCommand "cat ygp.text.ptb | ./word_count | awk '{print $1}' >> all_words.duplicate"
  callCommand "cat all_words.duplicate | ./word_count | awk '{print $1}' >> all_words"
  putStrLn "all_words produced"

  -- From Wikidata JSON dump, produces wikidata.items which is the first source of all wikidata data
  callCommand "pigz -dc /opt/wikidata-20170206-all.json.gz | ./wikidata_etl config.ygp.json >wikidata.items"
  putStrLn "wikidata.items produced"
  
  callCommand "cat wikidata.items | awk -F $'\t' 'NF==5{print $3}' > wikidata.items.P31"
  callCommand "cat wikidata.items | awk -F $'\t' 'NF==5{print $4}' > wikidata.items.P279" -- Not used for now
  callCommand "cat wikidata.items.P31 | tr ' ' '_' | ./word_count > wikidata.items.P31.count"
  callCommand "cat wikidata.items.P279 | tr ' ' '_' | ./word_count > wikidata.items.P279.count" -- Not used for now
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};NF==5{print $3 \"\t\" $1}'> items.uid"
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};{print \"WIKIDATAITEM_\" $1 \"\t\" $NF}' > wikidata.ner_input"
  putStrLn "wikidata analysis completed"
  
  let corenlpEnv = "/data/groups/uphere/parsers/corenlp"
  -- callCommand $ "java -mx48g edu.stanford.nlp.ie.NERClassifierCombiner -ner.model "++ corenlpEnv ++"/classifiers/english.all.3class.distsim.crf.ser.gz,"++ corenlpEnv ++"/classifiers/english.conll.4class.distsim.crf.ser.gz,"++ corenlpEnv ++"/classifiers/english.muc.7class.distsim.crf.ser.gz -textFile wikidata.ner_input > wikidata.ner" -- very time-consuming
  putStrLn "corenlp parsing for wikidata completed"
  
  callCommand "./count > items.by_p31"
  callCommand "./corenlp_ner  > wikidata.is_sfne"
  callCommand "cat wikidata.is_sfne | awk 'BEGIN {FS=\"\t\"};{print $1 \"\t\" $NF}' > wikidata.uid.is_sfne"
  callCommand "./ne_by_property > wikidata.p31.is_ne"
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};NF==5{print $1 \"\t\" $3  \"\t\" $NF}' > wikidata.names"
  callCommand "./wikidata_ner > wikidata.nes"
  callCommand "./count > items.by_p31"

  putStrLn "wikidata NER completed"

  callCommand "cat wikidata.nes | awk -F '\t' '$2==\"True\"{print $1}' > wikidata.uid.ne"
  callCommand "cat wikidata.items | awk -F '\t' 'NF==5{print $1}' > wikidata.uid"
  callCommand "cat wikidata.items | awk -F '\t' '{print $1 \"\t\" $NF}' > wikidata.all_entities"
  callCommand "cat wikidata.items | awk -F '\t' 'NF==5{print $1 \"\t\" $3}' > wikidata.properties"

  putStrLn "wikidata items produced"


  createDirectoryIfMissing True "YGP.json"
  -- callCommand "find /opt/YGP.text -type f | xargs -P20 -I {} python ../../rnn++/scripts/corenlp.py {} YGP.json/" -- time-consuming
  putStrLn "corenlp parsing for YGP completed"

  -- callCommand "find YGP.json/  -name '*.*.*.*' > ygp.corenlp" -- This is original command
  callCommand "find /opt/YGP.json/  -name '*.*.*.*' > ygp.corenlp"
  let minorVersion = ("0" :: String)
  callCommand $ "make -j20 && time ./ygpdb_etl config.ygp.json ygp.corenlp " ++ minorVersion
  putStrLn "indexing YGP completed"
  
  callCommand "./word_importance_build"

runRSS :: IO ()
runRSS = do  
  -- Write JSON config files.
  defYGP <- getDefYGP
  defRSS <- getDefRSS
  
  BL.writeFile "config.ygp.json" (encode defYGP)
  BL.writeFile "config.rss.json" (encode defRSS)
    
  -- Build apps in corenlp.
  callCommand "ghc -o count ../../corenlp/wiki/count.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o corenlp_ner ../../corenlp/wiki/corenlp_ner.hs ../../corenlp/wiki/corenlp.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o ne_by_property ../../corenlp/wiki/ne_by_property.hs ../../corenlp/wiki/wikidata.hs"
  callCommand "ghc -o wikidata_ner ../../corenlp/wiki/wikidata_ner.hs ../../corenlp/wiki/wikidata.hs"
  putStrLn "corenlp app build completed"
  
  -- Dump the YGP DB, and produce all_words
  callCommand "./ygpdb_dump /data/groups/uphere/similarity_test/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > ygp.text.ptb"
  callCommand "cat ygp.text.ptb | ./word_count | awk '{print $1}' >> all_words.duplicate"
  callCommand "cat all_words.duplicate | ./word_count | awk '{print $1}' >> all_words"
  putStrLn "all_words produced"

  -- From Wikidata JSON dump, produces wikidata.items which is the first source of all wikidata data
  callCommand "pigz -dc /opt/wikidata-20170206-all.json.gz | ./wikidata_etl config.ygp.json >wikidata.items"
  putStrLn "wikidata.items produced"
  
  callCommand "cat wikidata.items | awk -F $'\t' 'NF==5{print $3}' > wikidata.items.P31"
  callCommand "cat wikidata.items | awk -F $'\t' 'NF==5{print $4}' > wikidata.items.P279" -- Not used for now
  callCommand "cat wikidata.items.P31 | tr ' ' '_' | ./word_count > wikidata.items.P31.count"
  callCommand "cat wikidata.items.P279 | tr ' ' '_' | ./word_count > wikidata.items.P279.count" -- Not used for now
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};NF==5{print $3 \"\t\" $1}'> items.uid"
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};{print \"WIKIDATAITEM_\" $1 \"\t\" $NF}' > wikidata.ner_input"
  putStrLn "wikidata analysis completed"
  
  let corenlpEnv = "/data/groups/uphere/parsers/corenlp"
  -- callCommand $ "java -mx48g edu.stanford.nlp.ie.NERClassifierCombiner -ner.model "++ corenlpEnv ++"/classifiers/english.all.3class.distsim.crf.ser.gz,"++ corenlpEnv ++"/classifiers/english.conll.4class.distsim.crf.ser.gz,"++ corenlpEnv ++"/classifiers/english.muc.7class.distsim.crf.ser.gz -textFile wikidata.ner_input > wikidata.ner" -- very time-consuming
  putStrLn "corenlp parsing for wikidata completed"
  
  callCommand "./count > items.by_p31"
  callCommand "./corenlp_ner  > wikidata.is_sfne"
  callCommand "cat wikidata.is_sfne | awk 'BEGIN {FS=\"\t\"};{print $1 \"\t\" $NF}' > wikidata.uid.is_sfne"
  callCommand "./ne_by_property > wikidata.p31.is_ne"
  callCommand "cat wikidata.items | awk 'BEGIN {FS=\"\t\"};NF==5{print $1 \"\t\" $3  \"\t\" $NF}' > wikidata.names"
  callCommand "./wikidata_ner > wikidata.nes"
  callCommand "./count > items.by_p31"

  putStrLn "wikidata NER completed"

  callCommand "cat wikidata.nes | awk -F '\t' '$2==\"True\"{print $1}' > wikidata.uid.ne"
  callCommand "cat wikidata.items | awk -F '\t' 'NF==5{print $1}' > wikidata.uid"
  callCommand "cat wikidata.items | awk -F '\t' '{print $1 \"\t\" $NF}' > wikidata.all_entities"
  callCommand "cat wikidata.items | awk -F '\t' 'NF==5{print $1 \"\t\" $3}' > wikidata.properties"

  putStrLn "wikidata items produced"


  createDirectoryIfMissing True "YGP.json"
  -- callCommand "find /opt/YGP.text -type f | xargs -P20 -I {} python ../../rnn++/scripts/corenlp.py {} YGP.json/" -- time-consuming
  putStrLn "corenlp parsing for YGP completed"

  -- callCommand "find YGP.json/  -name '*.*.*.*' > ygp.corenlp" -- This is original command
  callCommand "find /opt/YGP.json/  -name '*.*.*.*' > ygp.corenlp"
  let minorVersion = ("0" :: String)
  callCommand $ "make -j20 && time ./ygpdb_etl config.ygp.json ygp.corenlp " ++ minorVersion
  putStrLn "indexing YGP completed"
  
  callCommand "./word_importance_build"
