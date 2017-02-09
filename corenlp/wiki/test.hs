{-# LANGUAGE OverloadedStrings #-}
import           System.IO
import qualified Data.HashMap.Strict as HM
import           Text.Printf
import           Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Lex.Fractional 
import           Data.Monoid

score_file = "/data/groups/uphere/ontology/wikidata/word_importance"
entity_file = "/data/groups/uphere/ontology/wikidata/wikidata.labels.single_word"

parse_score :: [BL.ByteString] -> (BL.ByteString, Double)
parse_score [x,y] = (x, fromMaybe 0.0 (fmap fst (readDecimal (BL.toStrict y)) ))
parse_score line = error (show line)

-- parse_entity (x:xs) = (x, head xs )
parse_entity [x,y] = (x,y )
parse_entity line = error (show line)

read_scores :: FilePath -> IO( HM.HashMap BL.ByteString Double )
read_scores score_file = do                  
  score_str <- BL.readFile score_file
  return $ HM.fromList $ ((map parse_score . map BL.words . BL.lines) score_str)

main = do
  entity_str <- BL.readFile entity_file
  scoring <- read_scores score_file
  let entities = (map parse_entity . map BL.words . BL.lines) entity_str
      outputs =  map (\(uid, word) ->(uid, word, fromMaybe 0.0 (HM.lookup word scoring) ))  entities
      known_entities = filter (\(_,_,score) -> score>0.0) outputs
  mapM_ (\(uid,word,score) -> BL.putStrLn (uid <>" "<> word <>" "<> BL.pack (show score))) $ known_entities
