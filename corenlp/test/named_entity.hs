{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified NamedEntity                as N
import qualified CoreNLP                    as C

parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  N.parseStr word tag

main = do
  let 
    oscar    = parseStanfordNE (C.parseNERToken "Oscar/PERSON")
    munoz    = N.parseStr "Munoz" "PERSON"
    united   = N.parseStr "United" "ORG"
    airlines = N.parseStr "Airlines" "ORG"
  print oscar
  print munoz
  print united
  print airlines
  print (N.mergeToken united airlines)
  print (N.mergeTokens [united, airlines, oscar, munoz, united, munoz])
  --print (N.collapseToken united munoz)

