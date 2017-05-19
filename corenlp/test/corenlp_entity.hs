{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Maybe                        (fromJust)
import           Data.List                         (inits)
import           NamedEntity                       (NamedEntity)
import           Assert                            (assert)
import qualified NamedEntity                as N
import qualified CoreNLP                    as C
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import qualified EntityLinking              as L

parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  N.parseStr word tag




-- For internal testing. User should not use these functions directly:
testNEParsing = do
  assert (N.NamedEntityFrag "Oscar" N.Person   == parseStanfordNE (C.parseNERToken "Oscar/PERSON"))
  assert (N.NamedEntity "Oscar Munoz" N.Person == fromJust (N.mergeToken (map parseStanfordNE (C.parseNEROutputStr "Oscar/PERSON Munoz/PERSON"))))
  assert (N.NamedEntity "United Airlines" N.Org == fromJust (N.mergeToken [N.parseStr "United" "ORGANIZATION", N.parseStr "Airlines" "ORGANIZATION"]))
  assert (N.NamedEntityFrag "United Airlines" N.Org == N.parseStr "United Airlines" "ORGANIZATION")  


testContextedEntityLinking = do
  let
    oscarMunoz = N.NamedEntity "Oscar Munoz" N.Person
    munoz      = N.NamedEntity "Munoz" N.Person
    munozOrg   = N.NamedEntity "Munoz" N.Org

    e0 = N.OrderedNamedEntity 0 oscarMunoz
    e1 = N.OrderedNamedEntity 1 munoz

    f0 = N.OrderedNamedEntity 0 munoz
    f1 = N.OrderedNamedEntity 1 oscarMunoz
    
  assert (L.mayRefer munoz oscarMunoz)
  assert (not (L.mayRefer munozOrg oscarMunoz))
  assert (L.canRefer e1 e0)
  assert (not (L.canRefer f0 f1))

main = do
  let 
    corenlp_output = "Oscar/PERSON Munoz/PERSON is/O a/O CEO/O of/O United/ORGANIZATION Airlines/ORGANIZATION ./O Munoz/PERSON apologized/O to/O Dao/PERSON ./O"
    tokens   = map parseStanfordNE (C.parseNEROutputStr corenlp_output)
  assert (N.mergeTokens tokens == [N.NamedEntity "Oscar Munoz" N.Person, N.NamedEntity "United Airlines" N.Org, N.NamedEntity "Munoz" N.Person, N.NamedEntity "Dao" N.Person])
  testNEParsing
  testContextedEntityLinking
