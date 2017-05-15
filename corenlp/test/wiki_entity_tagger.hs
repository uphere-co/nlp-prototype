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

itemTuple :: [a] -> (a,a)
itemTuple [uid,name] = (uid,name)

--readEntityNames :: Text -> IO [(Text,Text)]
readEntityNames filename = do
    content <- T.IO.readFile "../rnn++/tests/data/wikidata.test.entities"
    let
      --entities = map (itemTuple . T.split (=='\t')) (T.lines content)
      entities = map (T.split (=='\t')) (T.lines content)
    return entities

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
