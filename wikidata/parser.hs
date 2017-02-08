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

data TopLevel = TopLevel { toplevel_id :: Text
                         , toplevel_type :: Text
                         , toplevel_labels :: Value
                         , toplevel_descriptions :: Value
                         , toplevel_aliases :: Value
                         , toplevel_claims :: Value
                         , toplevel_sitelinks :: Value
                         , toplevel_lastrevid :: Maybe Int
                         , toplevel_modified :: Maybe Text
                         } deriving (Show,Eq)

instance FromJSON TopLevel where
  parseJSON (Object o) = TopLevel <$> (o .: "id")
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

  let x = evalState (runEitherT (listChunk 10000)) lbstr
  case x of
    Left str -> print str
    Right xs -> print (length xs)

  {- 
  let lbstr = BL.drop 1 lbstr0 

  -- let lbstr  = " { \"a\" : \"b\" }, "

  let r = A.parse json lbstr
  -- print r
  case r of
    A.Fail _ _ _ -> print "fail"
    A.Done _ v -> do
      let x :: AT.Result TopLevel = AT.parse parseJSON v
      print x
      {- 
      case v of
        Object o -> do
          print "object"
          
          print (HM.size o)
          print (HM.keys o)
        _ -> print "other"
      -}
 -}
{- 
  let mv :: Either String Value = eitherDecode lbstr

  case mv of
    Left err -> putStrLn err -- return ()
    Right v -> print v -- (head v) -- (head vs)

-}
  {- 
    let lbstr' = BL.filter (\x -> x == '{' || x ==  '}') lbstr

      sleft = BL.filter (== '{') lbstr'
      sright = BL.filter (== '}') lbstr'
  print (BL.length sleft)
  print (BL.length sright)
  
  -}
