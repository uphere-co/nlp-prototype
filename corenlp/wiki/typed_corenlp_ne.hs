{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           System.IO
import qualified Data.HashMap.Strict as HM
import           Text.Printf
import           Data.Maybe (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Text.Read                    (rational)
import qualified Data.Text.Encoding         as T.E
import qualified Data.Text.IO               as T.IO
import           Data.Monoid
import           Data.List                         (foldl', all)

data NamedEntity = Org    { _Org    :: Text}
                 | Person { _Person :: Text}
                 | Loc    { _Loc    :: Text}
                 | Time   { _Time   :: Text}
                 deriving(Show, Eq)

parseStr :: Text -> Text -> NamedEntity
parseStr str t | t== "PERSON"   = Person str
               | t== "ORG"      = Org str
               | t== "LOCATION" = Loc str
               | t== "TIME"     = Time str

main = do
  print (parseStr "Oscar" "PERSON")
  print (parseStr "United" "ORG")
