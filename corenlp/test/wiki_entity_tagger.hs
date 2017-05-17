{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Maybe                            (fromJust, isNothing)
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

ithElementOrdering :: (Ord e) => Int -> [e] -> [e] -> Ordering
ithElementOrdering i lhs rhs | length lhs <= i = LT
                             | length rhs <= i = GT
                             | otherwise = compare (lhs!!i) (rhs!!i)

binarySearchLR :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> e -> m (Int,Int)
binarySearchLR vec elm = do
  idxL <- VS.binarySearchL vec elm
  idxR <- VS.binarySearchR vec elm
  return (idxL, idxR)  

binarySearchLRBy :: (PrimMonad m, MVector v e) => VS.Comparison e -> v (PrimState m) e -> e -> m (Int,Int)
binarySearchLRBy comp vec elm = do
  idxL <- VS.binarySearchLBy comp vec elm
  idxR <- VS.binarySearchRBy comp vec elm
  return (idxL, idxR)  

binarySearchLRByBounds :: (PrimMonad m, MVector v e) => VS.Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m (Int,Int)
binarySearchLRByBounds comp vec elm l u = do
  idxL <- VS.binarySearchLByBounds comp vec elm l u
  idxR <- VS.binarySearchRByBounds comp vec elm l u
  return (idxL, idxR)  

--greedyMatch [] words = Nothing
greedyMatch :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> e -> m (Maybe [e])
greedyMatch entities words = do
  let
    f x y | x == y = Nothing
  (idxL, idxR) <- binarySearchLR entities words
  return (f idxL idxR)

testNameOrdering = do
  assert (ithElementOrdering 0 ["A", "B"] ["B", "A"] == LT)
  assert (ithElementOrdering 1 ["A", "B"] ["B", "A"] == GT)
  assert (ithElementOrdering 1 ["A", "A"] ["A", "A", "A"] == EQ)
  
testBinarySearch = do
  let
    wordss = V.fromList ([["B"], ["B", "C"], ["B", "B"], ["B","C","B"],  ["A","B"], ["A"], ["B"], ["B"], ["A", "C"], ["C"],["C"], ["C", "B"], ["E","A"], ["E"], ["G"]] :: [[Text]])
    wordssSorted = [["A"],["A","B"],["A","C"],["B"],["B"],["B"],["B","B"],["B","C"],["B","C","B"],["C"],["C"],["C","B"], ["E"], ["E","A"], ["G"]]
  
  tt <- V.thaw wordss
  VA.sort tt
  ttSorted <- V.freeze tt
  assert (V.toList ttSorted == wordssSorted)
  
  (idxBL, idxBR) <- binarySearchLR tt ["B"]
  (idxCL, idxCR) <- binarySearchLR tt ["C"]
  assert ((idxBL,idxBR) == (3,6))
  assert ((idxCL,idxCR) == (9,11))
  
  (idxDL, idxDR) <- binarySearchLR tt ["D"]
  assert ((idxDL,idxDR) == (12,12))
  (idxDL0, idxDR0) <- binarySearchLRBy (ithElementOrdering 0) tt ["D"]
  assert ((idxDL,idxDR) == (12,12))

  (bidxBL0, bidxBR0) <- binarySearchLRBy (ithElementOrdering 0) tt ["B", "C"]
  assert ((bidxBL0, bidxBR0)==(3,9))
  (bidxBL1, bidxBR1) <- binarySearchLRByBounds (ithElementOrdering 1) tt ["B", "C"] bidxBL0 bidxBR0
  assert ((bidxBL1, bidxBR1)==(7,9))
  (_, _) <- binarySearchLRByBounds (ithElementOrdering 1) tt ["B", "C"] 3 6
  

  (tl0, tr0) <- binarySearchLRBy (ithElementOrdering 0) tt ["E", "B"]
  assert ((tl0, tr0)==(12,14))
  (tl1, tr1) <- binarySearchLRByBounds (ithElementOrdering 1) tt ["E", "B"] tl0 tr0
  assert ((tl1, tr1)==(14,14))

testGreedyMatching = do
  let 
    entities = V.fromList ([["A"], ["A", "B"], ["A","B","C"]] :: [[Text]])
    words    = ["X", "A","B", "Z"] :: [Text]

  es <- V.thaw entities
  r <- greedyMatch es (["X"] :: [Text])
  assert (isNothing r)  

main = do
  testBinarySearch
  testNameOrdering
  testGreedyMatching

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
