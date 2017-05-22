{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
  
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList)
import           Assert                                (assert,massertEqual,eassertEqual)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup)
import qualified Data.Text                     as T 

import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger                      (buildEntityTable,wikiAnnotator)
import           WikiNamedEntityTagger                 (getStanfordNEs,parseStanfordNE,namedEntityAnnotator)
import           CoreNLP                               (parseNEROutputStr)
-- For testing:
import           Misc                                  (IRange(..))
import qualified NamedEntity                   as N
import qualified WikiEntity                    as Wiki


testNamedEntityTagging = testCaseSteps "Named entity tagging on CoreNLP NER output" $ \step -> do
  entities <- do
     reprs <- loadEntityReprs "../rnn++/tests/data/wikidata.test.entities"
     return (buildEntityTable reprs)  
  let
    ner_text = "Google/ORGANIZATION and/O Facebook/ORGANIZATION Inc./ORGANIZATION are/O famous/O AI/O companies/O ./O NLP/ORGANIZATION stands/O for/O natural/O language/O processing/O ./O"
    stanford_nefs =  map parseStanfordNE (parseNEROutputStr ner_text)
    matchedItems  = namedEntityAnnotator entities stanford_nefs
    uids = fromList . map Wiki.UID
    expected_matches = [(IRange 12 15, uids ["Q30642"], N.Other),
                        (IRange 9 10,  uids ["Q30642"], N.Other),
                        (IRange 6 7,   uids ["Q42970", "Q11660"], N.Other),
                        (IRange 2 4,   uids ["Q380"], N.Other),
                        (IRange 0 1,   uids ["Q95", "Q9366"], N.Other)]

    tt = getStanfordNEs stanford_nefs
    expected_tt = [(IRange 9 10, N.Org),(IRange 2 4,N.Org),(IRange 0 1, N.Org)]
  print ner_text
  eassertEqual tt expected_tt
  eassertEqual matchedItems expected_matches

unitTests =
  testGroup
    "All Unit tests"
    [testNamedEntityTagging]    

main = defaultMain unitTests
