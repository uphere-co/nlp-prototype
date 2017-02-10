{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- import           Control.Concurrent               (forkIO)
import           Control.Concurrent.Async
import           Control.Monad                    -- (forever,guard,join,replicateM,when)
import           Control.Monad.IO.Class           (MonadIO(..))
import           Control.Monad.Loops              (whileJust_,whileJust)
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
import           Data.List.Split                  (chunksOf)
import           Data.Maybe                       (maybeToList, listToMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Format           as TF
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as TL
-- import qualified Orc

import           System.IO                          (Handle,IOMode(..),withFile)
--
import           WikiData.Type

{-
count2 :: (MonadIO m) => Int -> Sink a m ()
count2 !n = do
  let m = 1000
  liftIO $ print n
  CL.drop m
  CL.peek >>= \case Nothing -> return ()
                    Just _ -> count (n+m)
-}

count :: (MonadIO m) => Int -> Sink a m ()
count !n = 
  (await >>=) $ mapM_ $ \_ -> do
    when (n `mod` 1000 == 0) $ liftIO $ print n
    count (n+1)

parseItem :: ByteString -> Either String TopLevel
parseItem str = 
  case A.parse json (BL.fromStrict str) of
    A.Fail _ _ msg -> Left msg
    A.Done str' v -> do
      let x :: AT.Result TopLevel = AT.parse parseJSON v
      case x of
        AT.Error msg -> Left msg
        AT.Success v -> Right v

extractProp :: (MonadIO m) => Either String TopLevel -> m [T.Text]
extractProp etl =
  case etl of
    Left err -> liftIO $ putStrLn err >> return []
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
      return $! map (\x -> TL.toStrict (TF.format "{},{},{}\n" x)) lst

main = do
  withFile "test.txt" WriteMode $ \h -> do
    runResourceT $ 
      CB.sourceFile "/data/groups/uphere/ontology/wikidata/wikidata-20170206-all.json" $$
        CB.lines =$= CL.isolate 100000 =$ withCounter (go h)
 where
  process chk = do
    xss <- CL.sourceList chk $$
             whileJust_ await (yield . parseItem) =$= whileJust await extractProp
    return $! map T.concat xss
   
  go h = do
    CL.chunksOf 100000 =$ do 
      whileJust_ await $ \bunch -> liftIO $ do
        xss <- forConcurrently (chunksOf 5000 bunch) process
        mapM_ (TIO.hPutStr h . T.concat) xss

withCounter action = getZipSink (ZipSink (count 0) *> ZipSink action)

