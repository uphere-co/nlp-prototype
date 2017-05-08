{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified NamedEntity                as N
import qualified CoreNLP                    as C

parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  N.parseStr word tag

main = do
  let 
    entity = parseStanfordNE (C.parseNERToken "Oscar/PERSON")
  print entity
  print (N.parseStr "Munoz" "PERSON")   
  print (N.parseStr "United" "ORG")
  