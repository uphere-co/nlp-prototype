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
import qualified Wikidata                   as W
import qualified CoreNLP                    as C

nerFile    = "wikidata.ner"
-- nerFile    = "aa"


newtype Tag  = Tag { unTag :: Text}
             deriving (Show, Eq, Ord)

newtype EntityStr = EntityStr { unEntityStr :: Text}
data WikidataEntity = WikidataEntity { name :: W.Name
                                     , tag  :: Tag
                                     , uid  :: W.UID }
                    deriving (Show)

mergeWordToken :: C.WordToken -> C.WordToken -> C.WordToken
mergeWordToken (C.WordToken word1) (C.WordToken word2) = C.WordToken (word1<>" "<>word2)

mergeSNEtag :: C.NETag -> C.NETag -> C.NETag
mergeSNEtag (C.NETag tag1) (C.NETag tag2) = C.NETag (tag1 <> "_" <> tag2 )

mergeSNEToken :: C.EntityToken -> C.EntityToken -> C.EntityToken
mergeSNEToken (C.EntityToken word1 tag1) (C.EntityToken word2 tag2) = C.EntityToken (mergeWordToken word1 word2) (mergeSNEtag tag1 tag2)

mergeSNETokens :: [C.EntityToken] -> C.EntityToken
mergeSNETokens [] = C.EntityToken (C.WordToken "_ERROR_") (C.NETag "O")
mergeSNETokens ts = foldl' mergeSNEToken x ys
                   where x:ys = ts

parseNERToken :: Text -> C.EntityToken
parseNERToken tokenStr = (\(x,y)-> (C.EntityToken (C.WordToken (T.dropEnd 1 x)) (C.NETag y))) $ T.breakOnEnd (T.pack "/") tokenStr

parseEntity :: EntityStr -> WikidataEntity
parseEntity (EntityStr entityStr) = WikidataEntity (W.Name name) (Tag tag) (W.UID uid)
                                  where
                                    C.EntityToken (C.WordToken uid) _ : ts = map parseNERToken (T.words entityStr)
                                    C.EntityToken (C.WordToken name) (C.NETag tag) = mergeSNETokens ts


splitTokens :: Text -> [EntityStr]
splitTokens str = tail (map EntityStr (T.splitOn "WIKIDATAITEM_" str))

isNamedEntity :: WikidataEntity -> Text
isNamedEntity (WikidataEntity _ (Tag tag) _) = f bool
                                           where
                                             ts = T.split (=='_') tag
                                             bool = not $ any (== "O") ts
                                             f False = "False"
                                             f True  = "True"


serializeWE (WikidataEntity (W.Name name) (Tag tag) (W.UID uid)) =  uid <> "\t" <> name  <> "\t" <> tag
main = do
  nerStr <- T.IO.readFile nerFile
  let wes = map parseEntity (splitTokens nerStr)
  mapM_ (T.IO.putStrLn . (\we -> serializeWE we <> "\t" <> isNamedEntity we)) wes
