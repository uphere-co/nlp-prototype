{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Maybe                            (fromJust)
import           Data.List                             (inits, transpose)
import           Data.Text                             (Text)
import           Control.Monad.Primitive               (PrimMonad, PrimState)
import           Data.Vector.Generic.Mutable.Base      (MVector)
import           Data.Ord                              (Ord)
import           Assert                                (assert)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Vector.Mutable           as MV
import qualified Data.Vector                   as V
{-
import qualified Data.Vector.Unboxed.Mutable   as MV
import qualified Data.Vector.Unboxed           as V
-}
import qualified Data.Vector.Algorithms.Intro  as VA
import qualified Data.Vector.Algorithms.Search as VS



itemTuple :: [Text] -> (Text,[Text])
itemTuple [uid,name] = (uid, T.words name)

--readEntityNames :: Text -> IO [(Text,Text)]
readEntityNames filename = do
    content <- T.IO.readFile "../rnn++/tests/data/wikidata.test.entities"
    let
      entities = map (T.split (=='\t')) (T.lines content)
    return entities

uidOrdering (lhsUid, lhsName) (rhsUid, rhsName) 
  | lhsUid <  rhsUid = LT
  | lhsUid == rhsUid = EQ
  | lhsUid >  rhsUid = GT

-- Sort names. Longer names come first for greedy matching.
nameOrdering (lhsUid, lhsName) (rhsUid, rhsName) 
  | lhsName >  rhsName = LT
  | lhsName == rhsName = EQ
  | lhsName <  rhsName = GT

binarySearchLR :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> e -> m (Int,Int)
binarySearchLR vec elm = do
  idxL <- VS.binarySearchL vec elm
  idxR <- VS.binarySearchR vec elm
  return (idxL, idxR)

  

main = do
  let 
    vec = V.fromList ([5,3,1,2,6,3,9,9,6,4,6] :: [Int])
    items = V.fromList (["A", "A"] :: [Text])

    wordss = V.fromList ([["B"], ["B", "B"], ["B","B","B"],  ["A","B"], ["A"], ["B"], ["B"], ["A", "C"], ["C"],["C"], ["C", "A"]] :: [[Text]])
    wordssSorted = [["A"],["A","B"],["A","C"],["B"],["B"],["B"],["B","B"],["B","B","B"],["C"],["C"],["C","A"]]
  tt <- V.thaw wordss
  VA.sort tt
  idxB <- VS.binarySearch tt ["B"]
  idxBL <- VS.binarySearchL tt ["B"]
  (idxCL, idxCR) <- binarySearchLR tt ["C"]
  idxD <- VS.binarySearchR tt ["D"]
  ttSorted <- V.freeze tt
  print (V.toList ttSorted == wordssSorted)
  print ttSorted
  print idxB
  print idxBL
  print (idxCL,idxCR)
  print idxD
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
  VA.sortBy uidOrdering mvecEntities
  entitiesByUID <- V.freeze mvecEntities
  print "Sorted by UID:"  
  print entitiesByUID

  VA.sortBy nameOrdering mvecEntities
  entitiesByName <- V.freeze mvecEntities
  print "Sorted by name:"  
  print entitiesByName
