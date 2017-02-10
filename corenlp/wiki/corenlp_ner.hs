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

-- nerFile    = "wikidata.names.single_word.ner"
nerFile    = "ners"

parseNERToken token = (\(x,y)-> (T.dropEnd 1 x, y))$ T.breakOnEnd (T.pack "/") token
parseTokensPerLine line = map parseNERToken (T.words line)

main = do
  nerStr <- T.IO.readFile nerFile
  let nes = concatMap parseTokensPerLine (T.lines nerStr)
  mapM_ (\(word, nerTag) -> T.IO.putStrLn (word <> " " <> nerTag) ) nes
