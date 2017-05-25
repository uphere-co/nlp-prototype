{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module StanfordNE where

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

{-
nerFile    = "wikidata.ner"
nerFile    = "aa"
-}
nerFile    = "wikidata.ner"


newtype EntityStr = EntityStr { unEntityStr :: Text}

toWikidataEntity :: W.UID -> C.EntityToken -> W.Entity
toWikidataEntity uid (C.EntityToken (C.WordToken word) (C.NETag tag))
  = W.Entity (W.Name word) (W.NETag tag) uid

mergeWordToken :: W.Name -> C.WordToken ->W.Name
mergeWordToken (W.Name word1) (C.WordToken word2) = W.Name (word1<>" "<>word2)

mergeSNEtag :: W.NETag -> C.NETag -> W.NETag
mergeSNEtag (W.NETag tag1) (C.NETag tag2) = W.NETag (tag1 <> "_" <> tag2 )

mergeSNEToken :: W.Entity -> C.EntityToken -> W.Entity
mergeSNEToken (W.Entity word1 tag1 uid) (C.EntityToken word2 tag2) 
  = W.Entity (mergeWordToken word1 word2) (mergeSNEtag tag1 tag2) uid

mergeSNETokens :: W.UID -> [C.EntityToken] -> W.Entity
mergeSNETokens uid [] = toWikidataEntity uid (C.EntityToken (C.WordToken "_ERROR_") (C.NETag "O"))
mergeSNETokens uid ts = foldl' mergeSNEToken (toWikidataEntity uid x) ys
                   where x:ys = ts

parseEntity :: EntityStr -> W.Entity
parseEntity (EntityStr entityStr) = mergeSNETokens uid ts
                                  where
                                    C.EntityToken (C.WordToken uidStr) _ : ts = map C.parseNERToken (T.words entityStr)
                                    uid = W.UID uidStr


splitTokens :: Text -> [EntityStr]
splitTokens str = tail (map EntityStr (T.splitOn "WIKIDATAITEM_" str))

isNamedEntity :: W.Entity -> Text
isNamedEntity (W.Entity _ (W.NETag tag) _) = f bool
                                           where
                                             ts = T.split (=='_') tag
                                             bool = not $ any (== "O") ts
                                             f False = "False"
                                             f True  = "True"


serializeWE (W.Entity (W.Name name) (W.NETag tag) (W.UID uid)) =  uid <> "\t" <> name  <> "\t" <> tag
main = do
  nerStr <- T.IO.readFile nerFile
  let wes = map parseEntity (splitTokens nerStr)
  mapM_ (T.IO.putStrLn . (\we -> serializeWE we <> "\t" <> isNamedEntity we)) wes
