{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified NamedEntity                as N
import qualified CoreNLP                    as C
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO


parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  N.parseStr word tag

main = do
  let 
    corenlp_output = "United/ORGANIZATION Airlines/ORGANIZATION -LRB-/O UAL.N/O -RRB-/O and/O its/O chief/O executive/O faced/O mounting/O pressure/O on/O Tuesday/DATE from/O a/O worldwide/O backlash/O over/O its/O treatment/O of/O a/O passenger/O who/O was/O dragged/O from/O his/O seat/O on/O a/O plane/O on/O Sunday/DATE to/O make/O room/O for/O four/O employees/O on/O the/O overbooked/O flight/O ./O \n\
\Lawyers/O for/O the/O passenger/O ,/O Dr./O David/PERSON Dao/PERSON ,/O issued/O a/O statement/O late/O on/O Tuesday/DATE confirming/O his/O identity/O and/O saying/O that/O he/O and/O his/O family/O were/O ``/O focused/O only/O on/O Dr./O Dao/PERSON 's/O medical/O care/O and/O treatment/O ''/O in/O a/O Chicago/LOCATION hospital/O ./O \n\
\The/O U.S./ORGANIZATION Department/ORGANIZATION of/ORGANIZATION Transportation/ORGANIZATION launched/O an/O inquiry/O into/O the/O incident/O ,/O and/O New/LOCATION Jersey/LOCATION Governor/O Chris/PERSON Christie/PERSON called/O for/O new/O rules/O to/O curb/O the/O airline/O practice/O of/O overbooking/O flights/O ./O \n\
\United/ORGANIZATION CEO/O Oscar/PERSON Munoz/PERSON issued/O a/O statement/O on/O Tuesday/DATE apologizing/O to/O Dao/PERSON without/O naming/O him/O ./O \n\
\``/O I/O 'm/O sorry/O ./O \n\
\We/O will/O fix/O this/O ,/O ''/O Munoz/PERSON said/O ./O \n\
\``/O I/O deeply/O apologize/O to/O the/O customer/O forcibly/O removed/O and/O to/O all/O the/O customers/O aboard/O ./O \n\
\No/O one/O should/O ever/O be/O mistreated/O this/O way/O ./O ''/O \n\
\On/O Monday/DATE ,/O Munoz/PERSON issued/O a/O memo/O to/O employees/O defending/O the/O company/O but/O not/O apologizing/O to/O the/O passenger/O ./O \n\
\Munoz/PERSON ,/O a/O former/O railroad/O executive/O who/O took/O over/O the/O helm/O at/O United/ORGANIZATION in/O 2015/DATE ,/O had/O already/O been/O under/O pressure/O from/O activist/O investors/O to/O improve/O the/O airline/O 's/O performance/O ,/O including/O its/O customer/O relations/O ./O \n\
\"
    tokens   = map parseStanfordNE (C.parseNEROutputStr corenlp_output)
    oscar    = parseStanfordNE (C.parseNERToken "Oscar/PERSON")
    munoz    = N.parseStr "Munoz" "PERSON"
    united   = N.parseStr "United" "ORGANIZATION"
    airlines = N.parseStr "Airlines" "ORGANIZATION"
  T.IO.putStrLn corenlp_output
  print tokens
  print oscar
  print munoz
  print united
  print airlines
  print (N.mergeToken united airlines)
  let entities = [united, airlines, oscar, munoz, united, munoz]
  print entities
  print (N.mergeTokens entities)
  print (N.mergeTokens tokens)
  --print (N.collapseToken united munoz)
