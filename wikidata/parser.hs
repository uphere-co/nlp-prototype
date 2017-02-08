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

data TopLevel = TopLevel { toplevel_id :: Text
                         , toplevel_type :: Text
                         , toplevel_labels :: HM.HashMap Text LangValue
                         , toplevel_descriptions :: HM.HashMap Text LangValue
                         , toplevel_aliases :: HM.HashMap Text [LangValue]
                         , toplevel_claims :: Value
                         , toplevel_sitelinks :: Value
                         , toplevel_lastrevid :: Maybe Int
                         , toplevel_modified :: Maybe Text
                         } deriving (Show,Eq)

instance FromJSON TopLevel where
  parseJSON (Object o) =
    TopLevel <$> (o .: "id")
             <*> (o .: "type")
             <*> (o .: "labels" >>= \(Object l) -> traverse parseJSON l)
             <*> (o .: "descriptions" >>= \(Object d) -> traverse parseJSON d)
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

  let x = evalState (runEitherT (listChunk 1)) lbstr
  case x of
    Left str -> print str
    Right ys -> do
      let y = head ys
      print (toplevel_labels y)
      -- mapM_ print xs -- (length xs)

