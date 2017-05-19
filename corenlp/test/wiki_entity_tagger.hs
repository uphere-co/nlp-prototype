{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
  
import           Data.Maybe                            (fromJust, isNothing)
import           Data.List                             (inits, transpose)
import           Data.Text                             (Text)
import           Control.Monad.Primitive               (PrimMonad, PrimState)
import           Control.Monad.ST                      (ST, runST)
import           Data.Vector.Generic.Mutable           (MVector)
import           Data.Vector                           (Vector,backpermute,findIndices
                                                       ,slice,fromList,toList,unsafeThaw,modify)
import           Data.Ord                              (Ord)
import           Assert                                (massertEqual,eassertEqual)
import           Test.Tasty.HUnit                      (assertBool,assertEqual, testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Search as VS
{-
import qualified Data.Vector.Unboxed.Mutable   as MV
import qualified Data.Vector.Unboxed           as V
-}


itemTuple :: [Text] -> (Text,[Text])
itemTuple [uid,name] = (uid, T.words name)

readEntityNames :: Text -> IO [[Text]]
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

data IRange = IRange { beg :: Int
                     , end :: Int}
                deriving(Eq, Show)

greedyMatchImpl :: (Ord e) => Vector [e] -> [e] -> (Int, IRange) -> (Int, IRange)
greedyMatchImpl entities words (i, IRange beg end) = runST $ do
  mvec <- unsafeThaw entities
  (idxL, idxR) <- binarySearchLRByBounds (ithElementOrdering i) mvec words beg end
  --return (i, IRange beg end)
  if idxL==idxR
    then return (i, IRange beg end)
    else return (greedyMatchImpl entities words (i+1, IRange idxL idxR))

greedyMatch :: (Ord e) => Vector [e] -> [e] -> (Int, IRange)
greedyMatch entities words = greedyMatchImpl entities words (0, IRange 0 (length entities))

getMatchedIndexes :: Vector [e] -> (Int, IRange) -> (Int, Vector Int)
getMatchedIndexes vec (len, IRange beg end) = (len, matchedItems)
  where 
    tmp          = findIndices (\x-> length x == len) vec
    matchedItems = V.filter (\x-> x>=beg && x<end) tmp

greedyMatchedItems :: (Ord e) => Vector [e] -> [e] -> (Int, Vector Int)
greedyMatchedItems entities words = getMatchedIndexes entities (greedyMatch entities words)

greedyAnnotationImpl :: (Ord e) => Vector [e] -> [e] -> Int -> [(IRange, Vector Int)] -> [(IRange, Vector Int)]
greedyAnnotationImpl entities []   offset results = results
greedyAnnotationImpl entities text offset results = 
  let
    (len, matched) = greedyMatchedItems entities text
    r = (IRange offset (offset+len), matched)
  in
    if len==0 || null matched
      then greedyAnnotationImpl entities (tail text) (offset+1) results
      else greedyAnnotationImpl entities (drop len text) (offset+len) (r:results)
  
greedyAnnotation :: (Ord e) => Vector [e] -> [e] -> [(IRange, Vector Int)]
greedyAnnotation entities text = greedyAnnotationImpl entities text 0 []


testVectorSlicing = testCaseSteps "API usages for vector slicing" $ \step -> do
  let 
    vec = fromList ([[1],[2],[3,4],[5,6],[7]] :: [[Int]])
    sub = slice 1 3 vec
  eassertEqual (toList (slice 1 1 vec)) [[2]]
  eassertEqual (toList (slice 2 2 vec)) [[3,4],[5,6]]
  eassertEqual (toList sub) [[2],[3,4],[5,6]]
  eassertEqual (filter (\x -> length x == 2) (toList sub)) [[3,4],[5,6]]

testBinarySearch = testCaseSteps "API usages for binary searches" $ \step -> do
  let
    wordss = fromList ([["B"], ["B", "C"], ["B", "B"], ["B","C","B"],  ["A","B"], ["A"], ["B"], ["B"], ["A", "C"], ["C"],["C"], ["C", "B"], ["E","A"], ["E"], ["G"]] :: [[Text]])
    wordssSorted = [["A"],["A","B"],["A","C"],["B"],["B"],["B"],["B","B"],["B","C"],["B","C","B"],["C"],["C"],["C","B"], ["E"], ["E","A"], ["G"]] :: [[Text]]
  
  tt <- V.thaw wordss
  sort tt  
  massertEqual (V.freeze tt) (fromList wordssSorted)
  
  step "binarySearchLR"
  massertEqual (binarySearchLR tt ["B"]) (3,6)
  massertEqual (binarySearchLR tt ["C"]) (9,11)  
  massertEqual (binarySearchLR tt ["D"]) (12,12)
  massertEqual (binarySearchLRBy (ithElementOrdering 0) tt ["D"]) (12,12)
  
  step "binarySearchLRBy"
  (bidxBL0, bidxBR0) <- binarySearchLRBy (ithElementOrdering 0) tt ["B", "C"]
  eassertEqual (bidxBL0, bidxBR0) (3,9)
  massertEqual (binarySearchLRByBounds (ithElementOrdering 1) tt ["B", "C"] bidxBL0 bidxBR0) (7,9)
  (_, _) <- binarySearchLRByBounds (ithElementOrdering 1) tt ["B", "C"] 3 6  

  (tl0, tr0) <- binarySearchLRBy (ithElementOrdering 0) tt ["E", "B"]
  eassertEqual (tl0, tr0) (12,14)
  massertEqual (binarySearchLRByBounds (ithElementOrdering 1) tt ["E", "B"] tl0 tr0) (14,14)

unitTestsVector =
  testGroup
    "Usages for Data.Vector and Data.Vector.Algorithms"
    [testVectorSlicing, testBinarySearch]


testNameOrdering = testCaseSteps "Ordering of entity names(list of words)" $ \step -> do
  eassertEqual LT (ithElementOrdering 0 ["A", "B"] ["B", "A"])
  eassertEqual GT (ithElementOrdering 1 ["A", "B"] ["B", "A"])
  eassertEqual EQ (ithElementOrdering 1 ["A", "A"] ["A", "A", "A"])

testGreedyMatching = testCaseSteps "Greedy matching of two lists of words" $ \step -> do
  let 
    entities = fromList ([["A"], ["B"], ["B","C"], ["B","D","E"],["B","D","F"],["C"],["C","D","E","F"],["C","D","E","F"]] :: [[Text]])
    words    = ["X", "A","B", "Z"] :: [Text]
  step "Null cases"
  eassertEqual (greedyMatch entities [])    (0, IRange 0 8)
  step "Single word cases"
  eassertEqual (greedyMatch entities ["X"]) (0, IRange 0 8)
  eassertEqual (greedyMatch entities ["B"]) (1, IRange 1 5)
  step "Multi words cases"
  eassertEqual (greedyMatch entities ["B","C","X","Y"]) (2, IRange 2 3)
  eassertEqual (greedyMatch entities ["B","D","X","Y"]) (2, IRange 3 5)
  eassertEqual (greedyMatch entities ["B","D","E","F"]) (3, IRange 3 4)
  eassertEqual (greedyMatch entities ["C","D","E","F"]) (4, IRange 6 8)

  step "Single run for entity tagging"
  eassertEqual (greedyMatchedItems entities ["B","C","X","Y","Z"]) (2, fromList [2])
  eassertEqual (greedyMatchedItems entities ["X", "B","C","X","Y","Z"]) (0, fromList [])
  
  step "Recursive tagging"
  let
    text = ["X", "B","C","X","Y","Z", "A", "B","D","F", "X","C","D","C","D","E","F","B"]
    expected = [(IRange 17 18, fromList [1])
               ,(IRange 13 17, fromList [6,7])
               ,(IRange 7 10,  fromList [4])
               ,(IRange 6 7,   fromList [0])
               ,(IRange 1 3,   fromList [2])]
  eassertEqual (greedyAnnotation entities text) expected
  --massertEqual (greedyMatchedItems entities ["X", "B","C","X","Y","Z"]) [(1,3, [["B","C"]])]
  {-
  massertEqual (greedyMatchedItems entities ["B","D","X","Y","Z"]) []
  massertEqual (greedyMatchedItems entities ["B","D","E","F","Z"]) [["B","D","E"]]
  massertEqual (greedyMatchedItems entities ["C","D","E","F","Z"]) [["C","D","E","F"],["C","D","E","F"]]
  -}
  


unitTestsGreedyMatching =
  testGroup
    "Text based, greedy matching algorithm for list of words"
    [testNameOrdering, testGreedyMatching]

unitTests =
  testGroup
    "All Unit tests"
    [unitTestsVector, unitTestsGreedyMatching]    

main = defaultMain unitTests

main1 = do
  entities <- readEntityNames "../rnn++/tests/data/wikidata.test.entities"
  let 
    [uids, names] =  transpose entities
    entitiesByUID = modify (sortBy uidOrdering) (fromList (map itemTuple entities))
    entitiesByName = modify (sortBy nameOrdering) (fromList (map itemTuple entities))
  print entities
  print uids
  print names  
  print "Sorted by UID:"  
  print entitiesByUID
  print "Sorted by name:"  
  print entitiesByName
