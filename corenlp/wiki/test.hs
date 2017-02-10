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


scoreFile = "/data/groups/uphere/ontology/wikidata/word_importance"
--scoreFile = "single"
-- entityFile = "/data/groups/uphere/ontology/wikidata/wikidata.labels.single_word"
entityFile = "single_words"

data Scoring = Scoring  { scores :: HM.HashMap Text Double}
                           deriving (Show)

extractDouble (Right (d, _)) = d

parseScore :: [Text] -> (Text, Double)
parseScore [x,y] = (x, extractDouble (rational y))
parseScore line = error (show line)

getScore :: Scoring -> Text -> Double
getScore (Scoring scores) word = fromMaybe 0.0 mscore
                          where mscore = HM.lookup word scores

readScores :: FilePath -> IO( Scoring )
readScores scoreFile = do                  
  scoreStr <- T.IO.readFile scoreFile
  return $ Scoring (HM.fromList $ map (parseScore . (T.words)) (T.lines scoreStr))


data Entity = Entity { uid  :: Text
                     , name :: Text }
parseEntity [x,y] = Entity x y
parseEntity line = error (show line)

readEntities entityFile = do
  entityStr <- T.IO.readFile entityFile
  return $ map (parseEntity . T.words) (T.lines entityStr)

main = do
  scores   <- readScores scoreFile  
  entities <- readEntities entityFile
  let scoring  = getScore scores
      outputs  =  map (\(Entity uid word) ->(uid, word, scoring word))  entities
      knownEntities = filter (\(_,_,score) -> score>0.0) outputs
  mapM_ (\(uid,word,score) -> T.IO.putStrLn (uid <>" "<> word <>" "<> T.pack (show score))) $ knownEntities
