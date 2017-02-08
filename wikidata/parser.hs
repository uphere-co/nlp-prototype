{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad                    (guard,join,replicateM)
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
import           Data.Maybe                       (maybeToList, listToMaybe)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Format           as TF
import qualified Data.Text.Lazy.IO          as TLIO
--
import           WikiData.Type

extractTopN :: Int -> EitherT String (State BL.ByteString) [TopLevel]
extractTopN n = do
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

  let x = evalState (runEitherT (extractTopN 1000)) lbstr
  case x of
    Left str -> print str
    Right ys -> do
      let lst = do y <- ys
                   let t = toplevel_type y
                   -- guard (t /= "item")
                   let ml = englishLabel y
                   l <- maybeToList ml
                   c <- concatMap (take 1) (HM.elems (toplevel_claims y))
                   let s = claim_mainsnak c
                       p = snak_property s
                   return (l,t,p)
      mapM_ (TF.print "{},{},{}\n") lst

englishLabel :: TopLevel -> Maybe Text
englishLabel = fmap lv_value . listToMaybe . filter (\l -> lv_language l == "en") . HM.elems . toplevel_labels 


