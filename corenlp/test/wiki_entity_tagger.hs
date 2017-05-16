{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Maybe                        (fromJust)
import           Data.List                         (inits, transpose)
import           Data.Text                         (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T.IO
import qualified Data.Vector.Mutable          as MV
import qualified Data.Vector                  as V
{-
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Unboxed          as V
-}
import qualified Data.Vector.Algorithms.Intro as VA

itemTuple :: [Text] -> (Text,[Text])
itemTuple [uid,name] = (uid, T.words name)

--readEntityNames :: Text -> IO [(Text,Text)]
readEntityNames filename = do
    content <- T.IO.readFile "../rnn++/tests/data/wikidata.test.entities"
    let
      entities = map (T.split (=='\t')) (T.lines content)
    return entities

sortByUid (lhsUid, lhsName) (rhsUid, rhsName) 
  | lhsUid <  rhsUid = LT
  | lhsUid >= rhsUid = GT

-- Sort names. Longer names come first for greedy matching.
sortByName (lhsUid, lhsName) (rhsUid, rhsName) 
  | lhsName >  rhsName = LT
  | lhsName <= rhsName = GT

main = do
  let 
    vec = V.fromList ([5,3,1,2,6,3,9,9,6,4,6] :: [Int])
    items = V.fromList (["A", "A"] :: [Text])

    wordss = V.fromList ([["B"], ["B", "B"], ["B","B","B"],  ["A","B"], ["A"], ["A", "C"], ["C"], ["C", "A"]] :: [[Text]])
  tt <- V.thaw wordss
  VA.sort tt
  ttSorted <- V.freeze tt
  print ttSorted
  print "----------"


  mvec <- V.unsafeThaw vec
  VA.sort mvec
  vec2  <- V.unsafeFreeze mvec
  print vec2

  entities <- readEntityNames "../rnn++/tests/data/wikidata.test.entities"
  let 
    [uids, names] =  transpose entities
  print entities
  print uids
  print names

  mvecEntities <- V.thaw (V.fromList (map itemTuple entities))
  VA.sortBy sortByUid mvecEntities
  entitiesByUID <- V.freeze mvecEntities
  print "Sorted by UID:"  
  print entitiesByUID

  VA.sortBy sortByName mvecEntities
  entitiesByName <- V.freeze mvecEntities
  print "Sorted by name:"  
  print entitiesByName
