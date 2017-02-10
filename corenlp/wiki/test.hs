{-# LANGUAGE OverloadedStrings #-}
import           System.IO
import qualified Data.HashMap.Strict as HM
import           Text.Printf
import           Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.ByteString.Lex.Fractional 
import           Data.Monoid

-- scoreFile = "/data/groups/uphere/ontology/wikidata/word_importance"
scoreFile = "single"
entityFile = "/data/groups/uphere/ontology/wikidata/wikidata.labels.single_word"

parseScore :: [BL.ByteString] -> (BL.ByteString, Double)
parseScore [x,y] = (x, fromMaybe 0.0 (fmap fst (readDecimal (BL.toStrict y)) ))
parseScore line = error (show line)

-- parseEntity (x:xs) = (x, head xs )
parseEntity [x,y] = (x,y )
parseEntity line = error (show line)

readScores :: FilePath -> IO( HM.HashMap BL.ByteString Double )
readScores scoreFile = do                  
  scoreStr <- BL.readFile scoreFile
  return $ HM.fromList $ (map parseScore . map BL.words . BL.lines) scoreStr

main = do
  entityStr <- BL.readFile entityFile
  scoring <- readScores scoreFile
  -- let entities = (map parseEntity . map BL.words . BL.lines) entityStr
  let entities = (map parseEntity . map (BL.split ' ') . BL.lines) entityStr
      outputs =  map (\(uid, word) ->(uid, word, fromMaybe 0.0 (HM.lookup word scoring) ))  entities
      knownEntities = filter (\(_,_,score) -> score>0.0) outputs
  mapM_ (\(uid,word,score) -> BL.putStrLn (uid <>" "<> word <>" "<> BL.pack (show score))) $ knownEntities
