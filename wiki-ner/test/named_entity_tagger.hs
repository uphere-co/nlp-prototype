{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
  
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList,toList)
import           Control.Arrow                         (first,second)
import           Assert                                (assert,massertEqual,eassertEqual)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Vector                   as V

import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger                      (buildEntityTable,wikiAnnotator)
import           WikiEntityClass                       (fromFiles,getNEClass)
import           WikiNamedEntityTagger                 (resolveNEs,buildTagUIDTable,getStanfordNEs,parseStanfordNE,namedEntityAnnotator)
import           WikiNamedEntityTagger                 (untilOverlapOrNo,untilNoOverlap,relativePos,PreNE(..),resolveNEClass)
import           CoreNLP                               (parseNEROutputStr)
-- For testing:
import           Misc                                  (IRange(..), isContain,subVector)
import qualified NamedEntity                   as N
import qualified WikiEntity                    as Wiki
import qualified WikiEntityClass               as WC

uid = Wiki.UID
uids = fromList . map uid


ai1 = (uid "Q42970", N.Other)
ai2 = (uid "Q11660", N.Other)
nlp = (uid "Q30642", N.Other)
google       = (uid "Q95", N.Org)
googleSearch = (uid "Q9366", N.Other)
facebook     = (uid "Q380", N.Org)

testNamedEntityTagging :: TestTree
testNamedEntityTagging = testCaseSteps "Named entity tagging on CoreNLP NER output" $ \step -> do
  entities <- do
     reprs <- loadEntityReprs "../rnn++/tests/data/wikidata.test.entities"
     return (buildEntityTable reprs)  
  let
    ner_text = "Google/ORGANIZATION and/O Facebook/ORGANIZATION Inc./ORGANIZATION are/O famous/O AI/O companies/O ./O NLP/ORGANIZATION stands/O for/O natural/O language/O processing/O ./O"
    stanford_nefs =  map parseStanfordNE (parseNEROutputStr ner_text)
    uid2tag = WC.fromList [nlp,ai1,ai2,facebook,google,googleSearch]
    matchedItems  = namedEntityAnnotator entities uid2tag stanford_nefs
    expected_matches = [(IRange 0 1,   fromList [google, googleSearch])
                       ,(IRange 2 4,   fromList [facebook])
                       ,(IRange 6 7,   fromList [ai1, ai2])
                       ,(IRange 9 10,  fromList [nlp])
                       ,(IRange 12 15, fromList [nlp])
                       ]
    tt = getStanfordNEs stanford_nefs
    expected_tt = [(IRange 0 1, N.Org),(IRange 2 4,N.Org),(IRange 9 10, N.Org)]

    others = buildTagUIDTable N.Other (uids ["Q11660","Q9366","Q30642"])
    orgs   = buildTagUIDTable N.Org   (uids ["Q42970","Q380","Q95"])    
    --TODO: sort UID
    uidTags = mconcat [others, orgs]
    
  --print uidTags
  --print ner_text
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
  eassertEqual (resolveNEClass N.Person ambiguousUID) (Resolved (uid "Q3", N.Person))

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
    
    r1 = resolveNEs stanford_nes entities1
    expected_r1 = [(IRange 0 2, Resolved (uid "Q13", N.Person)),
                   (IRange 5 8, AmbiguousUID [uid "Q22",uid "Q21"])]

    entities2 = [(IRange 0 2, ambiguousUID1),(IRange 5 7, ambiguousUID2)]
    r2 = resolveNEs stanford_nes entities2
    expected_r2 = [(IRange 0 2, Resolved (uid "Q13", N.Person)),
                   (IRange 5 8, UnresolvedUID N.Org)]

    entities3 = [(IRange 0 2, ambiguousUID1),(IRange 4 6, ambiguousUID2)]
    r3 = resolveNEs stanford_nes entities3
    expected_r3 = [(IRange 0 2, Resolved (uid "Q13", N.Person)),
                   (IRange 4 6, UnresolvedClass (toList ambiguousUID2))]

    entities4 = [(IRange 0 2, ambiguousUID1),(IRange 7 9, ambiguousUID2)]
    r4 = resolveNEs stanford_nes entities4
    expected_r4 = expected_r2

  print stanford_nes
  eassertEqual r1 expected_r1
  --eassertEqual r2 expected_r2
  --eassertEqual r3 expected_r3
  --eassertEqual r4 expected_r4  
  

testWikiNER :: TestTree
testWikiNER = 
  testGroup
    "Unit tests for WikiNER"
      [testNamedEntityTagging, testNEResolution]


testRunWikiNER :: TestTree
testRunWikiNER = testCaseSteps "Test run for Wiki named entity annotator" $ \step -> do
  input_raw <- T.IO.readFile "data/dao.ptb"
  input <- T.IO.readFile "data/dao.ner"
  uid2tag <- fromFiles [(N.Org, "data/ne.org"), (N.Person, "data/ne.person")]
  wikiTable <- do
     reprs <- loadEntityReprs "data/uid"
     return (buildEntityTable reprs)  
  
  let
    stanford_nefs = map parseStanfordNE (parseNEROutputStr input)
    named_entities =  filter (\x -> snd x == N.Org || snd x == N.Person) (getStanfordNEs stanford_nefs)
    flag1 = getNEClass uid2tag (uid "Q95")
    flag2 = getNEClass uid2tag (uid "Q3503829")
    wiki_entities = namedEntityAnnotator wikiTable uid2tag stanford_nefs
    wiki_named_entities = resolveNEs named_entities wiki_entities

    text = fromList (T.words input_raw)
    output = map (\(range,e) -> (range, subVector range text, e)) wiki_named_entities

    t1 = (IRange 1 3, fromList ["Oscar","Munoz"],Resolved (uid "Q21066734",N.Person))
    t2 = (IRange 10 11, fromList ["Munoz"], UnresolvedUID N.Person)


  print "Named entities"
  mapM_ print named_entities
  print "Wiki entities"
  mapM_ print wiki_entities
  print "Wiki named entities"
  mapM_ print wiki_named_entities      
  print "End"
  mapM_ print output

  print t1
  print t2


testHelperUtils :: TestTree
testHelperUtils = testCaseSteps "Test for helper functions on general algorithms" $ \step -> do
  assert $ isContain (fromList [1,2]) (fromList [1,2,3])
  assert $ not $ isContain (fromList [1,2]) (fromList [1])
  assert $ isContain (fromList [1,2]) (fromList [0,1,2,3])

unitTests :: TestTree
unitTests =
  testGroup
    "All Unit tests"
    [testHelperUtils, testIRangeOps, testWikiNER, testRunWikiNER]    

main = defaultMain unitTests
