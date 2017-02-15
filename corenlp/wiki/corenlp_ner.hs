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
import           Data.List                         (foldl', all)

-- nerFile    = "wikidata.ner"
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

newtype EntityStr = EntityStr { unEntityStr :: Text}
data WikidataEntity = WikidataEntity { name :: Name
                                     , tag  :: Tag
                                     , uid  :: WikidataUID }
                    deriving (Show)

mergeWordToken :: WordToken -> WordToken -> WordToken
mergeWordToken (WordToken word1) (WordToken word2) = WordToken (word1<>" "<>word2)

mergeSNEtag :: SNEtag -> SNEtag -> SNEtag
mergeSNEtag (SNEtag tag1) (SNEtag tag2) = SNEtag (tag1 <> "_" <> tag2 )

mergeSNEToken :: SNEToken -> SNEToken -> SNEToken
mergeSNEToken (SNEToken word1 tag1) (SNEToken word2 tag2) = SNEToken (mergeWordToken word1 word2) (mergeSNEtag tag1 tag2)

msergeSNETokens :: [SNEToken] -> SNEToken
msergeSNETokens ts = foldl' mergeSNEToken x ys
                   where x:ys = ts

parseNERToken :: Text -> SNEToken
parseNERToken tokenStr = (\(x,y)-> (SNEToken (WordToken (T.dropEnd 1 x)) (SNEtag y))) $ T.breakOnEnd (T.pack "/") tokenStr

parseEntity :: EntityStr -> WikidataEntity
parseEntity (EntityStr entityStr) = WikidataEntity (Name name) (Tag tag) (WikidataUID uid)
                                  where
                                    SNEToken (WordToken uid) _ : ts = map parseNERToken (T.words entityStr)
                                    SNEToken (WordToken name) (SNEtag tag) = msergeSNETokens ts


splitTokens :: Text -> [EntityStr]
splitTokens str = tail (map (\x -> EntityStr x) (T.splitOn "WIKIDATAITEM_" str))

isNamedEntity :: WikidataEntity -> Text
isNamedEntity (WikidataEntity _ (Tag tag) _) = f bool
                                           where
                                             ts = T.split (=='_') tag
                                             bool = not $ all (== "O") ts
                                             f False = "False"
                                             f True  = "True"


serializeWE (WikidataEntity (Name name) (Tag tag) (WikidataUID uid)) =  uid <> "\t" <> name  <> "\t" <> tag
main = do
  nerStr <- T.IO.readFile nerFile
  let wes = map parseEntity (splitTokens nerStr)
  mapM_ T.IO.putStrLn $ map (\we -> (serializeWE we <> "\t" <> isNamedEntity we)) wes
