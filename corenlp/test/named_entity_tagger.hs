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
import           WikiEntityTagger                      (IRange(IRange),buildEntityTable,wikiAnnotator)
import           WikiNamedEntityTagger                 (parseStanfordNE,namedEntityAnnotator)
import           CoreNLP                               (parseNEROutputStr)


testNamedEntityTagging = testCaseSteps "Named entity tagging on CoreNLP NER output" $ \step -> do
  entities <- do
     reprs <- loadEntityReprs "../rnn++/tests/data/wikidata.test.entities"
     return (buildEntityTable reprs)
  let
    ner_text = "Google/ORGANIZATION and/O Facebook/ORGANIZATION Inc./ORGANIZATION are/O famous/O AI/O companies/O ./O NLP/ORGANIZATION stands/O for/O natural/O language/O processing/O ./O"
    stanford_nefs =  map parseStanfordNE (parseNEROutputStr ner_text)
    matchedItems  = namedEntityAnnotator entities stanford_nefs
    
  print ner_text
  print matchedItems

unitTests =
  testGroup
    "All Unit tests"
    [testNamedEntityTagging]    

main = defaultMain unitTests
