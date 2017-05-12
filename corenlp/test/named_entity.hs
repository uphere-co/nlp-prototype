{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Maybe                        (fromJust)
import           NamedEntity                       (NamedEntity)
import qualified NamedEntity                as N
import qualified CoreNLP                    as C
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO

parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  N.parseStr word tag

assert :: Bool -> IO ()
assert True = return ()
assert False = error "Assertion error"

testNEParsing = do
  assert (N.NamedEntityFrag "Oscar" N.Person   == parseStanfordNE (C.parseNERToken "Oscar/PERSON"))
  assert (N.NamedEntity "Oscar Munoz" N.Person == fromJust (N.mergeToken (map parseStanfordNE (C.parseNEROutputStr "Oscar/PERSON Munoz/PERSON"))))
  -- For internal testing. User should not use parseStr directly:
  assert (N.NamedEntity "United Airlines" N.Org == fromJust (N.mergeToken [N.parseStr "United" "ORGANIZATION", N.parseStr "Airlines" "ORGANIZATION"]))
  assert (N.NamedEntityFrag "United Airlines" N.Org == N.parseStr "United Airlines" "ORGANIZATION")  
  
main = do
  let 
    corenlp_output = "Oscar/PERSON Munoz/PERSON is/O a/O CEO/O of/O United/ORGANIZATION Airlines/ORGANIZATION ./O Munoz/PERSON apologized/O to/O Dao/PERSON ./O"
    tokens   = map parseStanfordNE (C.parseNEROutputStr corenlp_output)
  assert (N.mergeTokens tokens == [N.NamedEntity "Oscar Munoz" N.Person, N.NamedEntity "United Airlines" N.Org, N.NamedEntity "Munoz" N.Person, N.NamedEntity "Dao" N.Person])
  testNEParsing
  --print (N.collapseToken united munoz)
