{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
  
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList)
import           Assert                                (assert,massertEqual,eassertEqual)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import qualified Data.Text                     as T 

import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger                      (buildEntityTable,wikiAnnotator)
import           WikiNamedEntityTagger                 (buildTagUIDTable,getStanfordNEs,parseStanfordNE,namedEntityAnnotator)
import           WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
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

testNETagResolution :: TestTree
testNETagResolution = testCaseSteps "Idenifying Wiki UID with Stanford NE tag" $ \step -> do
  let
    ambiguousEntity = fromList [(uid "Q1", N.Org), (uid "Q2", N.Org), (uid "Q3", N.Person)]
  eassertEqual (resolveNEClass N.Org ambiguousEntity) (AmbiguousUID [uid "Q2", uid "Q1"])
  eassertEqual (resolveNEClass N.Person ambiguousEntity) (Resolved (uid "Q3"))

unitTests :: TestTree
unitTests =
  testGroup
    "All Unit tests"
    [testNamedEntityTagging, testNETagResolution]    

main :: IO ()
main = defaultMain unitTests
