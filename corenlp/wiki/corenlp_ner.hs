{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
nerFile    = "aa"

newtype WordToken    = WordToken   { unWord :: Text}
                     deriving (Show, Eq, Ord)
newtype SNEtag  = SNEtag { unSNEtag :: Text}
                deriving (Show, Eq, Ord)

data SNETagged = SNETagged { word  :: WordToken
                           , neTag :: SNEtag }


parseNERToken token = (\(x,y)-> (SNETagged (WordToken (T.dropEnd 1 x)) (SNEtag y))) $ T.breakOnEnd (T.pack "/") token

parseTokensPerLine line = map parseNERToken (T.words line)

splitTokens str = tail (T.splitOn "WIKIDATAITEM_" str)

main = do
  nerStr <- T.IO.readFile nerFile
  let nes = concatMap parseTokensPerLine (splitTokens nerStr)
  mapM_ (\(SNETagged token tag) -> print (token, tag) ) nes
