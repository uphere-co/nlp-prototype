{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent               (forkIO)
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
import           Orc

import           System.IO                          (Handle,IOMode(..),withFile)
--
import           WikiData.Type

count :: (MonadIO m) => Int -> Sink a m ()
count !n =
  (await >>=) $ mapM_ $ \_ -> do
    when (n `mod` 1000 == 0) $ liftIO $ print n
    count (n+1)

-- extract1TL :: MonadResource m => Conduit ByteString m (Either String TopLevel)
parseItem :: ByteString -> Either String TopLevel
parseItem str = 
  -- whileJust_ await $ \str -> 
    case A.parse json (BL.fromStrict str) of
      A.Fail _ _ msg -> Left msg
      A.Done str' v -> do
        let x :: AT.Result TopLevel = AT.parse parseJSON v
        case x of
          AT.Error msg -> Left msg
          AT.Success v -> Right v

-- record1TL :: (MonadResource m, MonadIO m) => Sink (Either String TopLevel) m [[T.Text]]
extractProp :: (MonadIO m) => Either String TopLevel -> m [T.Text]
extractProp etl =
  -- whileJust await $ \etl ->
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


--  this function is defined in conduit 1.2.9. 
conduitChunksOf :: Monad m => Int -> Conduit a m [a]
conduitChunksOf n =
    start
  where
    start = await >>= maybe (return ()) (\x -> loop n (x:))

    loop !count rest =
        await >>= maybe (yield (rest [])) go
      where
        go y
            | count > 1 = loop (count - 1) (rest . (y:))
            | otherwise = yield (rest []) >> loop n (y:)

main = do
  withFile "test.txt" WriteMode $ \h -> do
    runResourceT $ 
      CB.sourceFile "/data/groups/uphere/wikidata/wikidata-20170206-all.json" $$
        CB.lines =$= CL.isolate 10000 =$ withCounter (process h)
 where
  process h = do
    conduitChunksOf 1000 =$ do 
      whileJust_ await $ \bunch -> liftIO $ do
        xss <- flip mapM (chunksOf 100 bunch) $ \chk -> do -- forkIO $ do
          xss <- CL.sourceList chk $$ whileJust_ await (yield . parseItem) =$= whileJust await extractProp
          return $! map T.concat xss
         --    xss <- whileJust_ await (yield . parseItem) =$= whileJust await extractProp
        mapM_ (TIO.hPutStr h . T.concat) xss



withCounter action = getZipSink (ZipSink (count 0) *> ZipSink action)

