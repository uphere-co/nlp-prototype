import System.IO
import qualified Data.HashMap.Strict as HM
import Text.Printf
import Data.Maybe (fromMaybe)

score_file = "/data/groups/uphere/ontology/wikidata/word_importance"
entity_file = "/data/groups/uphere/ontology/wikidata/wikidata.single_word"

parse_score [x,y] = (x, read y :: Double )

parse_entity (x:xs) = (x, head xs )
parse_entity line = error (show line)

main = do
  score_str <- readFile score_file
  entity_str <- readFile entity_file
  let scoring  = HM.fromList $ ((map parse_score . map words . lines) score_str)
      entities = (map parse_entity . map words . lines) entity_str
      outputs =  map (\(uid, word) ->(uid, word, fromMaybe 0.0 (HM.lookup word scoring) ))  entities
      known_entities = filter (\(_,_,score) -> score>0.0) outputs
  mapM_ (\(uid,word,score) -> printf "%s %s %f\n" uid word score) $ known_entities
