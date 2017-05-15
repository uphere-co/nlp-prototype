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
      --entities = map (itemTuple . T.split (=='\t')) (T.lines content)
      entities = map (T.split (=='\t')) (T.lines content)
    return entities

sortByUid (lhsUid, lhsName) (rhsUid, rhsName) 
  | lhsUid <  rhsUid = LT
  | lhsUid >= rhsUid = GT

main = do
  let 
    vec = V.fromList ([5,3,1,2,6,3,9,9,6,4,6] :: [Int])
    items = V.fromList (["A", "A"] :: [Text])
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
  vecEntities <- V.freeze mvecEntities
  print "Sorted:"
  print vecEntities
  sortedUIDs <- V.thaw (V.fromList uids)
  sortedNames <- V.thaw (V.fromList names)
  tmp <- V.freeze sortedUIDs
  tmp2 <- V.freeze sortedNames
  print tmp
  print tmp2