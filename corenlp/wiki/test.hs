{-# LANGUAGE OverloadedStrings #-}
import           System.IO
import qualified Data.HashMap.Strict as HM
import           Text.Printf
import           Data.Maybe (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Text.Read                    (rational)
import qualified Data.Text.Encoding         as T.E
import qualified Data.Text.IO               as T.IO
import           Data.Monoid


-- scoreFile = "/data/groups/uphere/ontology/wikidata/word_importance"
scoreFile = "single"
entityFile = "/data/groups/uphere/ontology/wikidata/wikidata.labels.single_word"

extractDouble (Right (d, _)) = d

parseScore :: [Text] -> (Text, Double)
parseScore [x,y] = (x, extractDouble (rational y))
parseScore line = error (show line)

-- parseEntity (x:xs) = (x, head xs )
parseEntity [x,y] = (x,y )
parseEntity line = error (show line)

readScores :: FilePath -> IO( HM.HashMap Text Double )
readScores scoreFile = do                  
  scoreStr <- T.IO.readFile scoreFile
  return $ HM.fromList $ (map parseScore . map T.words . T.lines) scoreStr

main = do
  entityStr <- T.IO.readFile entityFile
  scoring <- readScores scoreFile
  let entities = (map parseEntity . map T.words . T.lines) entityStr
      outputs =  map (\(uid, word) ->(uid, word, fromMaybe 0.0 (HM.lookup word scoring) ))  entities
      knownEntities = filter (\(_,_,score) -> score>0.0) outputs
  mapM_ (\(uid,word,score) -> T.IO.putStrLn (uid <>" "<> word <>" "<> T.pack (show score))) $ knownEntities
