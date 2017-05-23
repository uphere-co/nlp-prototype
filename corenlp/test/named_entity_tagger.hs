{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
  
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList,toList)
import           Assert                                (assert,massertEqual,eassertEqual)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import qualified Data.Text                     as T 

import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger                      (buildEntityTable,wikiAnnotator)
import           WikiNamedEntityTagger                 (resolveNEs,buildTagUIDTable,getStanfordNEs,parseStanfordNE,namedEntityAnnotator)
import           WikiNamedEntityTagger                 (untilOverlapOrNo,untilNoOverlap,relativePos,PreNE(..),resolveNEClass)
import           CoreNLP                               (parseNEROutputStr)
-- For testing:
import           Misc                                  (IRange(..))
import qualified NamedEntity                   as N
import qualified WikiEntity                    as Wiki



uid = Wiki.UID
uids = fromList . map uid
    

testNamedEntityTagging :: TestTree
testNamedEntityTagging = testCaseSteps "Named entity tagging on CoreNLP NER output" $ \step -> do
  entities <- do
     reprs <- loadEntityReprs "../rnn++/tests/data/wikidata.test.entities"
     return (buildEntityTable reprs)  
  let
    ner_text = "Google/ORGANIZATION and/O Facebook/ORGANIZATION Inc./ORGANIZATION are/O famous/O AI/O companies/O ./O NLP/ORGANIZATION stands/O for/O natural/O language/O processing/O ./O"
    stanford_nefs =  map parseStanfordNE (parseNEROutputStr ner_text)
    matchedItems  = namedEntityAnnotator entities stanford_nefs
    expected_matches = [(IRange 12 15, uids ["Q30642"], N.Other), -- NLP
                        (IRange 9 10,  uids ["Q30642"], N.Other), -- NLP
                        (IRange 6 7,   uids ["Q42970", "Q11660"], N.Other), -- AI
                        (IRange 2 4,   uids ["Q380"], N.Other), -- Facebook Inc.
                        (IRange 0 1,   uids ["Q95", "Q9366"], N.Other) -- Google
                        ]
    tt = getStanfordNEs stanford_nefs
    expected_tt = [(IRange 9 10, N.Org),(IRange 2 4,N.Org),(IRange 0 1, N.Org)]

    others = buildTagUIDTable N.Other (uids ["Q11660","Q9366","Q30642"])
    orgs   = buildTagUIDTable N.Org   (uids ["Q42970","Q380","Q95"])    
    --TODO: sort UID
    uidTags = mconcat [others, orgs]
    
  print uidTags
  print ner_text
  eassertEqual tt expected_tt
  eassertEqual matchedItems expected_matches

testIRangeOps :: TestTree
testIRangeOps = testCaseSteps "Test operations on IRange" $ \step -> do
  let
    ranges1 = [IRange 0 2, IRange 2 4, IRange 5 7, IRange 10 12]
    ranges2 = [IRange 0 2, IRange 2 4, IRange 7 8, IRange 10 12]
    ref = IRange 1 6
    dist_ref = relativePos ref
  eassertEqual (untilNoOverlap   dist_ref ranges1) [IRange 10 12]
  eassertEqual (untilNoOverlap   dist_ref ranges2) [IRange 7 8, IRange 10 12]
  eassertEqual (untilOverlapOrNo dist_ref ranges1) [IRange 5 7, IRange 10 12]
  eassertEqual (untilOverlapOrNo dist_ref ranges2) [IRange 7 8, IRange 10 12]


testNEResolution :: TestTree
testNEResolution = testCaseSteps "Resolving Wiki UID with Stanford NE tag" $ \step -> do
  let
    ambiguousUID = fromList [(uid "Q1", N.Org), (uid "Q2", N.Org), (uid "Q3", N.Person)]
    entities = [(IRange 1 4, ambiguousUID)]
  eassertEqual (resolveNEClass N.Org ambiguousUID) (AmbiguousUID [uid "Q2", uid "Q1"])
  eassertEqual (resolveNEClass N.Person ambiguousUID) (Resolved (uid "Q3"))

  step "Single entity cases"
  eassertEqual (resolveNEs [(IRange 1 4, N.Person)] entities) [(IRange 1 4, resolveNEClass N.Person ambiguousUID)]
  eassertEqual (resolveNEs [(IRange 1 4, N.Org)] entities) [(IRange 1 4, resolveNEClass N.Org ambiguousUID)]
  eassertEqual (resolveNEs [(IRange 1 2, N.Org)] entities) [(IRange 1 4, UnresolvedClass (toList ambiguousUID))]
  eassertEqual (resolveNEs [(IRange 0 5, N.Org)] entities) [(IRange 0 5, UnresolvedUID N.Org)]
  eassertEqual (resolveNEs [(IRange 0 2, N.Org)] entities) [(IRange 0 2, UnresolvedUID N.Org)]
  eassertEqual (resolveNEs [(IRange 3 5, N.Org)] entities) [(IRange 1 4, UnresolvedClass (toList ambiguousUID))]

  step "Multiple entities cases"
  let
    input = "A1/PERSON A2/PERSON x/O y/O z/O W1/ORGANIZATION W2/ORGANIZATION W3/ORGANIZATION"
    stanford_nes =  getStanfordNEs (map parseStanfordNE (parseNEROutputStr input))
    ambiguousUID1 = fromList [(uid "Q11", N.Org), (uid "Q12", N.Org), (uid "Q13", N.Person)]
    ambiguousUID2 = fromList [(uid "Q21", N.Org), (uid "Q22", N.Org), (uid "Q23", N.Person)]
    entities1 = [(IRange 0 2, ambiguousUID1),(IRange 5 8, ambiguousUID2)]
    r1 = resolveNEs (reverse stanford_nes) entities1
    expected_r1 = [(IRange 5 8, AmbiguousUID [uid "Q22",uid "Q21"]),
                   (IRange 0 2, Resolved (uid "Q13"))]

    entities2 = [(IRange 0 2, ambiguousUID1),(IRange 5 7, ambiguousUID2)]
    r2 = resolveNEs (reverse stanford_nes) entities2
    expected_r2 = [(IRange 5 8, UnresolvedUID N.Org),
                   (IRange 0 2, Resolved (uid "Q13"))]

    entities3 = [(IRange 0 2, ambiguousUID1),(IRange 4 6, ambiguousUID2)]
    r3 = resolveNEs (reverse stanford_nes) entities3
    expected_r3 = [(IRange 4 6, UnresolvedClass (toList ambiguousUID2)),
                   (IRange 0 2, Resolved (uid "Q13"))]               

    entities4 = [(IRange 0 2, ambiguousUID1),(IRange 7 9, ambiguousUID2)]
    r4 = resolveNEs (reverse stanford_nes) entities4
    expected_r4 = expected_r2

  eassertEqual r1 expected_r1
  eassertEqual r2 expected_r2
  eassertEqual r3 expected_r3
  eassertEqual r4 expected_r4  
  

testWikiNER :: TestTree
testWikiNER = 
  testGroup
    "Unit tests for WikiNER"
      [testNamedEntityTagging, testNEResolution]
      
unitTests :: TestTree
unitTests =
  testGroup
    "All Unit tests"
    [testIRangeOps, testWikiNER]    

main :: IO ()
main = defaultMain unitTests
