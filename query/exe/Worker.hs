{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Worker where

import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Monad
import           Control.Monad.IO.Class                    (MonadIO(liftIO))
import           Control.Monad.Trans.Maybe                 (MaybeT(MaybeT,runMaybeT))
import           Data.Aeson
import qualified Data.ByteString.Char8               as B
import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.ByteString.Unsafe                    (unsafePackCString)
import qualified Data.HashMap.Strict                 as HM
import           Data.Maybe                                (listToMaybe)
import           Data.Text                                 (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
import qualified Data.Text.IO                        as TIO
import           Foreign.ForeignPtr
--
import           QueryServer.Type
import           CoreNLP
import           JsonUtil

foreign import ccall "register_documents" c_register_documents :: Json_t -> IO Json_t
foreign import ccall "query"              c_query              :: Json_t -> IO Json_t

query :: Json -> IO Json
query q = withForeignPtr q c_query >>= newForeignPtr c_json_finalize

register_documents :: Json -> IO Json
register_documents q = withForeignPtr q c_register_documents >>= newForeignPtr c_json_finalize

registerText :: (MonadIO m) => Text -> MaybeT m RegisteredSentences
registerText txt = do
  guard ((not . T.null) txt)
  bstr_nlp <- (liftIO . runCoreNLP . TE.encodeUtf8) txt
  bstr0 <- liftIO $ B.useAsCString bstr_nlp $
    json_create >=> register_documents >=> json_serialize >=> unsafePackCString
  (MaybeT . return . decodeStrict') bstr0


queryRegisteredSentences :: RegisteredSentences -> IO BL.ByteString
queryRegisteredSentences r = do
  -- need to be configured.  
  let bstr = BL.toStrict $ encode (r { rs_max_clip_len = Just 200 })  
  bstr' <- B.useAsCString bstr $ 
    json_create >=> query >=> json_serialize >=> unsafePackCString
  return (BL.fromStrict bstr')


queryWorker :: TVar (HM.HashMap Text [Int]) -> SendPort BL.ByteString -> Query -> Process ()
queryWorker ref sc QueryText {..} = do
  m <- liftIO $ readTVarIO ref
  case HM.lookup query_text m of
    Just ids ->
      liftIO (queryRegisteredSentences RS { rs_sent_uids = ids, rs_max_clip_len = Nothing })
      >>= sendChan sc
    Nothing -> do
      r <- runMaybeT $ do
        r <- registerText query_text 
        resultbstr <- liftIO (queryRegisteredSentences r)
        liftIO $ atomically (modifyTVar' ref (HM.insert query_text (rs_sent_uids r)))
        return resultbstr
      case r of
        Just resultbstr -> sendChan sc resultbstr
        Nothing         -> sendChan sc failed 
queryWorker ref sc QueryRegister {..} = do
  m <- liftIO $ readTVarIO ref
  case HM.lookup query_register m of
    Just ids -> sendChan sc . encode $ RS { rs_sent_uids = ids, rs_max_clip_len = Nothing}
    Nothing -> do
      r <- runMaybeT $ do
        r <- registerText query_register
        liftIO $ atomically (modifyTVar' ref (HM.insert query_register (rs_sent_uids r)))
        let resultbstr = encode r
        return resultbstr
      case r of
        Just resultbstr -> sendChan sc resultbstr
        Nothing         -> sendChan sc failed 
queryWorker ref sc QueryById {..} = do
  m <- liftIO $ readTVarIO ref
  r <- runMaybeT $ do
    let rs = RS { rs_sent_uids = query_ids, rs_max_clip_len = Nothing}
    resultbstr <- liftIO (queryRegisteredSentences rs)
    return resultbstr
  case r of
    Just resultbstr -> sendChan sc resultbstr
    Nothing         -> sendChan sc failed 

