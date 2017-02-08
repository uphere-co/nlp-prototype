{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative              ((<|>))
import           Control.Monad                    (replicateM)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State        (State,runState,evalState,execState)
import           Control.Monad.State.Class
import           Data.Aeson
import qualified Data.Aeson.Types           as AT
import qualified Data.Attoparsec.Lazy       as A
import           Data.Attoparsec.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char                        (isSpace)
import qualified Data.HashMap.Strict        as HM
import           Data.Text                        (Text)
import qualified Data.Text                  as T

data LangValue = LV { lv_language :: Text
                    , lv_value :: Text
                    } deriving (Show, Eq)

instance FromJSON LangValue where
  parseJSON (Object o) = LV <$> (o .: "language")
                            <*> (o .: "value")
  parseJSON invalid = AT.typeMismatch "LangValue" invalid

data Snak = Snak { snak_snaktype :: Text
                 , snak_property :: Text
                 , snak_datatype :: Maybe Text
                 , snak_datavalue :: Maybe Value
                 } deriving (Show, Eq)

instance FromJSON Snak where
  parseJSON (Object o) = Snak <$> (o .: "snaktype")
                              <*> (o .: "property")
                              <*> ((Just <$> (o .: "datatype")) <|> return Nothing)
                              <*> ((Just <$> (o .: "datavalue")) <|> return Nothing)
  parseJSON invalid = AT.typeMismatch "Snak" invalid


data Claim = Claim { claim_id :: Text
                   , claim_mainsnak :: Snak
                   , claim_type :: Text
                   , claim_rank :: Text
                   , claim_qualifiers :: Maybe [Snak]
                   , claim_references :: Maybe [Value]
                   } deriving (Show, Eq)

instance FromJSON Claim where
  parseJSON (Object o) = Claim <$> (o .: "id")
                               <*> (o .: "mainsnak")
                               <*> (o .: "type")
                               <*> (o .: "rank")
                               <*> ((Just <$> (o .: "qualifiers")) <|> return Nothing)
                               <*> ((Just <$> (o .: "references")) <|> return Nothing)
  parseJSON invalid = AT.typeMismatch "Claim" invalid

data TopLevel = TopLevel { toplevel_id :: Text
                         , toplevel_type :: Text
                         , toplevel_labels :: HM.HashMap Text LangValue
                         , toplevel_descriptions :: HM.HashMap Text LangValue
                         , toplevel_aliases :: HM.HashMap Text [LangValue]
                         , toplevel_claims :: HM.HashMap Text [Claim]
                         , toplevel_sitelinks :: Value
                         , toplevel_lastrevid :: Maybe Int
                         , toplevel_modified :: Maybe Text
                         } deriving (Show,Eq)

instance FromJSON TopLevel where
  parseJSON (Object o) =
    TopLevel <$> (o .: "id")
             <*> (o .: "type")
             <*> (o .: "labels") 
             <*> (o .: "descriptions") 
             <*> (o .: "aliases") 
             <*> (o .: "claims")
             <*> (o .: "sitelinks")
             <*> ((Just <$> (o .: "lastrevid")) <|> return Nothing )
             <*> ((Just <$> (o .: "modified")) <|> return Nothing)
  parseJSON invalid = AT.typeMismatch "TopLevel" invalid
   

listChunk :: Int -> EitherT String (State BL.ByteString) [TopLevel]
listChunk n = do
  str <- get
  put (BL.tail str)
  replicateM n (parse1 <* skipSpc <* skipComma)
 where
  skipSpc = do
    str <- get
    put (BL.dropWhile isSpace str)
  skipComma = do
    str <- get
    put (BL.tail str)
  parse1 = do
    str <- get
    case A.parse json str of
      A.Fail _ _ msg -> left msg
      A.Done str' v -> do
        let x :: AT.Result TopLevel = AT.parse parseJSON v
        case x of
          AT.Error msg -> left msg
          AT.Success v -> put str' >> return v
    

main = do
  putStrLn "wikidata analysis"
  lbstr <- BL.readFile "/data/groups/uphere/wikidata/wikidata-20170206-all.json"

  let x = evalState (runEitherT (listChunk 100)) lbstr
  case x of
    Left str -> print str
    Right ys -> do
      let y = ys !! 99
      print (toplevel_claims y)
      -- mapM_ print xs -- (length xs)

