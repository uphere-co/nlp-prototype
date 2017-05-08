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

import qualified NamedEntity                as N
import qualified CoreNLP                    as C

parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  N.parseStr word tag

main = do
  let 
    entity = parseStanfordNE (C.parseNERToken "Oscar/PERSON")
  print entity
  print (N.parseStr "Munoz" "PERSON")   
  print ((N.parseStr "United" "ORG")==N.Org "United")