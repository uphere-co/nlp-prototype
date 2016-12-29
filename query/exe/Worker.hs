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
import           Foreign.C.String
import           Foreign.ForeignPtr
--
import           Query.Binding
import           Query.Binding.EngineWrapper
import           Query.Binding.Json
import           QueryServer.Type
import           CoreNLP

registerText :: (MonadIO m) => EngineWrapper -> Text -> MaybeT m RegisteredSentences
registerText engine txt = do
  guard ((not . T.null) txt)
  bstr_nlp <- (liftIO . runCoreNLP . TE.encodeUtf8) txt
  let bstr_txt = TE.encodeUtf8 txt
  bstr0 <- liftIO $
    B.useAsCString bstr_nlp $ \cstr_nlp -> 
      B.useAsCString bstr_txt $ \cstr_txt ->
        jsonparse cstr_nlp >>=
        register_documents engine cstr_txt >>=
        serialize >>=
        unsafePackCString
        {- json_create cstr_nlp >>=
        register_documents cstr_txt >>=
        json_serialize >>=
        unsafePackCString -}
  (MaybeT . return . decodeStrict') bstr0


queryRegisteredSentences :: EngineWrapper -> RegisteredSentences -> IO BL.ByteString
queryRegisteredSentences engine r = do
  -- need to be configured.  
  let bstr = BL.toStrict $ encode (r { rs_max_clip_len = Just 200 })  
  bstr' <- B.useAsCString bstr $
     jsonparse >=> query engine >=> serialize >=> unsafePackCString
    -- json_create >=> query >=> json_serialize >=> unsafePackCString
  return (BL.fromStrict bstr')

type ResultBstr = BL.ByteString

failed :: BL.ByteString
failed = encode Null


queryWorker :: TVar (HM.HashMap Text ([Int],[Text])) -> SendPort ResultBstr
            -> EngineWrapper
            -> Query
            -> Process ()
queryWorker ref sc engine QueryText {..} = do
  m <- liftIO $ readTVarIO ref
  case HM.lookup query_text m of
    Just (ids,countries) ->
      liftIO (queryRegisteredSentences engine RS { rs_sent_uids = ids
                                                 , rs_Countries = countries
                                                 , rs_max_clip_len = Nothing })
      >>= sendChan sc
    Nothing -> do
      r <- runMaybeT $ do
        r <- registerText engine query_text 
        resultbstr <- liftIO (queryRegisteredSentences engine r)
        liftIO $ atomically (modifyTVar' ref (HM.insert query_text (rs_sent_uids r,rs_Countries r)))
        return resultbstr
      case r of
        Just resultbstr -> sendChan sc resultbstr
        Nothing         -> sendChan sc failed 
queryWorker ref sc engine QueryRegister {..} = do
  m <- liftIO $ readTVarIO ref
  case HM.lookup query_register m of
    Just (ids,countries) ->
      sendChan sc . encode $ RS { rs_sent_uids = ids
                                , rs_Countries = countries
                                , rs_max_clip_len = Nothing}
    Nothing -> do
      r <- runMaybeT $ do
        r <- registerText engine query_register
        liftIO $ atomically (modifyTVar' ref (HM.insert query_register (rs_sent_uids r,rs_Countries r)))
        let resultbstr = encode r
        return resultbstr
      case r of
        Just resultbstr -> sendChan sc resultbstr
        Nothing         -> sendChan sc failed 
{-
-- comment out for the time being
queryWorker ref sc QueryById {..} = 

  do
  m <- liftIO $ readTVarIO ref
  r <- runMaybeT $ do
    let rs = RS { rs_sent_uids = query_ids, rs_max_clip_len = Nothing}
    resultbstr <- liftIO (queryRegisteredSentences rs)
    return resultbstr
  case r of
    Just resultbstr -> sendChan sc resultbstr
    Nothing         -> sendChan sc failed 
-}
