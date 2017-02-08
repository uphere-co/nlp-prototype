{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad                    -- (forever,guard,join,replicateM,when)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Loops              (whileJust_)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource     (MonadResource,runResourceT)
import           Control.Monad.Trans.State        (State,runState,evalState,execState)
import           Control.Monad.State.Class
import           Data.Aeson
import qualified Data.Aeson.Types           as AT
import qualified Data.Attoparsec.Lazy       as A
import           Data.Attoparsec.Types
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char                        (isSpace)
import           Data.Conduit
import qualified Data.Conduit.Binary        as CB (lines,sourceFile)
import qualified Data.Conduit.List          as CL 
import           Data.Foldable                    (forM_)
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe                       (maybeToList, listToMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Format           as TF
import qualified Data.Text.Lazy.IO          as TLIO
import           System.IO                          (Handle,IOMode(..),withFile)
--
import           WikiData.Type


{- 
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
-} 

count :: (MonadIO m) => Int -> Sink a m ()
count !n =
  (await >>=) $ mapM_ $ \_ -> do
    when (n `mod` 1000 == 0) $ liftIO $ print n
    count (n+1)

extract1TL :: MonadResource m => Conduit ByteString m (Either String TopLevel)
extract1TL = 
  whileJust_ await $ \str -> 
    case A.parse json (BL.fromStrict str) of
      A.Fail _ _ msg -> yield (Left msg)
      A.Done str' v -> do
        let x :: AT.Result TopLevel = AT.parse parseJSON v
        case x of
          AT.Error msg -> yield (Left msg)
          AT.Success v -> yield (Right v)

record1TL :: (MonadResource m, MonadIO m) => Handle -> Sink (Either String TopLevel) m ()
record1TL h =
  whileJust_ await $ \etl ->
    case etl of
      Left err -> liftIO $ putStrLn err
      Right y -> do
        let lst = do
              let t = toplevel_type y
              -- guard (t /= "item")
              let ml = englishLabel y
              l <- maybeToList ml
              c <- concatMap (take 1) (HM.elems (toplevel_claims y))
              let s = claim_mainsnak c
                  p = snak_property s
              return (l,t,p)
        forM_ lst $ \x -> liftIO (TLIO.hPutStr h (TF.format "{},{},{}\n" x))

main = do
  withFile "test.txt" WriteMode $ \h -> do
    runResourceT $ 
      CB.sourceFile "/data/groups/uphere/wikidata/wikidata-20170206-all.json" $$
        CB.lines =$= -- CL.isolate 100000 =$ 
          getZipSink ((,) <$> ZipSink (count 0) <*> ZipSink (extract1TL =$= record1TL h))
          -- CL.sinkNull



