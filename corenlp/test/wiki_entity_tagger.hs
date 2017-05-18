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
import           Data.Vector                           (Vector,backpermute, convert,fromList, modify)
import           Data.Ord                              (Ord)
import           Assert                                (massertEqual,eassertEqual)
import           Test.Tasty.HUnit                      (assertBool,assertEqual, testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T.IO
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Search as VS
{-
import qualified Data.Vector.Unboxed.Mutable   as MV
import qualified Data.Vector.Unboxed           as V
-}


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

data IndexRange = IndexRange { beg :: Int
                             , end :: Int}
                deriving(Eq, Show)

greedyMatchImpl :: (Ord e) => Vector [e] -> [e] -> Int -> IndexRange -> (Int, IndexRange)
greedyMatchImpl entities words i (IndexRange beg end) = runST $ do
  mvec <- V.unsafeThaw entities
  (idxL, idxR) <- binarySearchLRByBounds (ithElementOrdering i) mvec words beg end
  --return (i, IndexRange beg end)
  if idxL==idxR
    then return (i, IndexRange beg end)
    else return (greedyMatchImpl entities words (i+1) (IndexRange idxL idxR))

greedyMatch :: (Ord e) => Vector [e] -> [e] -> (Int, IndexRange)
greedyMatch entities words = greedyMatchImpl entities words 0 (IndexRange 0 (length entities))

getMatchedItems :: Vector [e] -> (Int, IndexRange) -> (Int, [[e]])
getMatchedItems vec (len, IndexRange beg end) = (len, matchedItems)
  where 
    sub = V.slice beg (end-beg) vec
    matchedItems = filter (\x-> length x == len) (V.toList sub)

greedyMatchedItems :: (Ord e) => Vector [e] -> [e] -> (Int, [[e]])
greedyMatchedItems entities words = getMatchedItems entities (greedyMatch entities words)

greedyAnnotationImpl :: (Ord e) => Vector [e] -> [e] -> Int -> [(Int, Int, [[e]])] -> [(Int, Int, [[e]])]
greedyAnnotationImpl entities []   offset results = results
greedyAnnotationImpl entities text offset results = 
  let
    (len, matched) = greedyMatchedItems entities text
    r = (offset, offset+len, matched)
  in
    if len==0 || null matched
      then greedyAnnotationImpl entities (tail text) (offset+1) results
      else greedyAnnotationImpl entities (drop len text) (offset+len) (r:results)
  
greedyAnnotation :: (Ord e) => Vector [e] -> [e] -> [(Int, Int, [[e]])]
greedyAnnotation entities text = greedyAnnotationImpl entities text 0 []
         
testVectorSlicing = testCaseSteps "API usages for vector slicing" $ \step -> do
  let 
    vec = V.fromList ([[1],[2],[3,4],[5,6],[7]] :: [[Int]])
    sub = V.slice 1 3 vec
  assertBool "" (V.toList (V.slice 1 1 vec) == [[2]])
  assertBool "" (V.toList (V.slice 2 2 vec) == [[3,4],[5,6]])
  assertBool "" (V.toList sub == [[2],[3,4],[5,6]])
  assertBool "" (filter (\x -> length x == 2) (V.toList sub) == [[3,4],[5,6]])

testBinarySearch = testCaseSteps "API usages for binary searches" $ \step -> do
  let
    wordss = V.fromList ([["B"], ["B", "C"], ["B", "B"], ["B","C","B"],  ["A","B"], ["A"], ["B"], ["B"], ["A", "C"], ["C"],["C"], ["C", "B"], ["E","A"], ["E"], ["G"]] :: [[Text]])
    wordssSorted = [["A"],["A","B"],["A","C"],["B"],["B"],["B"],["B","B"],["B","C"],["B","C","B"],["C"],["C"],["C","B"], ["E"], ["E","A"], ["G"]] :: [[Text]]
  
  tt <- V.thaw wordss
  sort tt  
  massertEqual (V.freeze tt) (V.fromList wordssSorted)
  
  step "binarySearchLR"
  massertEqual (binarySearchLR tt ["B"]) (3,6)
  massertEqual (binarySearchLR tt ["C"]) (9,11)  
  massertEqual (binarySearchLR tt ["D"]) (12,12)
  massertEqual (binarySearchLRBy (ithElementOrdering 0) tt ["D"]) (12,12)
  
  step "binarySearchLRBy"
  (bidxBL0, bidxBR0) <- binarySearchLRBy (ithElementOrdering 0) tt ["B", "C"]
  assertBool "" ((bidxBL0, bidxBR0)==(3,9))
  massertEqual (binarySearchLRByBounds (ithElementOrdering 1) tt ["B", "C"] bidxBL0 bidxBR0) (7,9)
  (_, _) <- binarySearchLRByBounds (ithElementOrdering 1) tt ["B", "C"] 3 6  

  (tl0, tr0) <- binarySearchLRBy (ithElementOrdering 0) tt ["E", "B"]
  assertBool "" ((tl0, tr0)==(12,14))
  massertEqual (binarySearchLRByBounds (ithElementOrdering 1) tt ["E", "B"] tl0 tr0) (14,14)

unitTestsVector =
  testGroup
    "Usages for Data.Vector and Data.Vector.Algorithms"
    [testVectorSlicing, testBinarySearch]


testNameOrdering = testCaseSteps "Ordering of entity names(list of words)" $ \step -> do
  assertBool "" (ithElementOrdering 0 ["A", "B"] ["B", "A"] == LT)
  assertBool "" (ithElementOrdering 1 ["A", "B"] ["B", "A"] == GT)
  assertBool "" (ithElementOrdering 1 ["A", "A"] ["A", "A", "A"] == EQ)

testGreedyMatching = testCaseSteps "Greedy matching of two lists of words" $ \step -> do
  let 
    entities = V.fromList ([["A"], ["B"], ["B","C"], ["B","D","E"],["B","D","F"],["C"],["C","D","E","F"],["C","D","E","F"]] :: [[Text]])
    words    = ["X", "A","B", "Z"] :: [Text]
  step "Null cases"
  eassertEqual (greedyMatch entities [])    (0, IndexRange 0 8)
  step "Single word cases"
  eassertEqual (greedyMatch entities ["X"]) (0, IndexRange 0 8)
  eassertEqual (greedyMatch entities ["B"]) (1, IndexRange 1 5)
  step "Multi words cases"
  assertBool "" (filter (\x -> length x == 2) (V.toList $ V.slice 1 6 entities) == [["B", "C"]])
  eassertEqual (greedyMatch entities ["B","C","X","Y"]) (2, IndexRange 2 3)
  eassertEqual (greedyMatch entities ["B","D","X","Y"]) (2, IndexRange 3 5)
  eassertEqual (greedyMatch entities ["B","D","E","F"]) (3, IndexRange 3 4)
  eassertEqual (greedyMatch entities ["C","D","E","F"]) (4, IndexRange 6 8)

  step "Single run for entity tagging"
  eassertEqual (greedyMatchedItems entities ["B","C","X","Y","Z"]) (2, [["B","C"]])
  eassertEqual (greedyMatchedItems entities ["X", "B","C","X","Y","Z"]) (0,[])
  
  step "Recursive tagging"
  let
    text = ["X", "B","C","X","Y","Z", "A", "B","D","F", "X","C","D","C","D","E","F"]
    expected = [(13,17,[["C","D","E","F"],["C","D","E","F"]]),(7,10,[["B","D","F"]]),(6,7,[["A"]]),(1,3,[["B","C"]])]
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
