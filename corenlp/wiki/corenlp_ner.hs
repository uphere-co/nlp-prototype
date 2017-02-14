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
import           Data.List                         (foldl')

-- nerFile    = "wikidata.names.single_word.ner"
nerFile    = "aa"

newtype WordToken    = WordToken   { unWord :: Text}
                     deriving (Show, Eq, Ord)
newtype SNEtag  = SNEtag { unSNEtag :: Text}
                deriving (Show, Eq, Ord)

data SNEToken = SNEToken { word  :: WordToken
                           , neTag :: SNEtag }


newtype Name = Name { unName :: Text}
             deriving (Show, Eq, Ord)
newtype Tag  = Tag { unTag :: Text}
             deriving (Show, Eq, Ord)
newtype WikidataUID  = WikidataUID { unWikidataUID :: Text}
                     deriving (Show, Eq, Ord)

data WikidataNE = WikidataNE { name :: Name
                             , tag  :: Tag
                             , uid  :: WikidataUID }
                deriving (Show)

mergeSNEToken :: SNEToken -> SNEToken -> SNEToken
mergeSNEToken (SNEToken (WordToken word1) (SNEtag tag1)) (SNEToken (WordToken word2) (SNEtag tag2)) 
  = SNEToken (WordToken (word1<>" "<>word2)) (SNEtag (tag1 <> "_" <> tag2 ))

msergeSNETokens :: [SNEToken] -> SNEToken
msergeSNETokens ts = foldl' mergeSNEToken x ys
                   where x:ys = ts

parseNERToken :: Text -> SNEToken
parseNERToken tokenStr = (\(x,y)-> (SNEToken (WordToken (T.dropEnd 1 x)) (SNEtag y))) $ T.breakOnEnd (T.pack "/") tokenStr

parseEntity :: Text -> WikidataNE
parseEntity entityStr = WikidataNE (Name name) (Tag tag) (WikidataUID uid)
                      where
                        SNEToken (WordToken uid) _ : ts = map parseNERToken (T.words entityStr)
                        SNEToken (WordToken name) (SNEtag tag) = msergeSNETokens ts

splitTokens :: Text -> [Text]
splitTokens str = tail (T.splitOn "WIKIDATAITEM_" str)

main = do
  nerStr <- T.IO.readFile nerFile
  let nes = map parseEntity (splitTokens nerStr)
  mapM_ T.IO.putStrLn (map (\(WikidataNE (Name name) (Tag tag) (WikidataUID uid)) -> (uid <> "\t" <> name  <> "\t" <> tag)) nes)
