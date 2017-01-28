{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Query.Binding.Json_t
import           QueryServer.Type
import           CoreNLP

registerText :: (MonadIO m) => EngineWrapper -> Text -> MaybeT m RegisteredSentences
registerText engine txt = do
  guard ((not . T.null) txt)
  bstr_nlp0 <- (liftIO . runCoreNLP . TE.encodeUtf8) txt
  bstr0 <- liftIO $
    B.useAsCString bstr_nlp0 $ \cstr_nlp0 -> do
      withCString "did_you_mean" $ \did_you_mean -> do
        cstr_nlp1 <- json_tparse cstr_nlp0 >>= preprocess_query engine >>= \j -> find j did_you_mean
        bstr_nlp1 <- unsafePackCString cstr_nlp1
        bstr_nlp2 <- runCoreNLP bstr_nlp1
        B.useAsCString bstr_nlp2 $ \cstr_nlp2 -> do
          r <- json_tparse cstr_nlp2 >>= register_documents engine cstr_nlp1 
          r' <- serialize r >>= unsafePackCString
          return $! r' 
  liftIO $ putStrLn "inside registerText"
  liftIO $ print bstr0
  (MaybeT . return . decodeStrict') bstr0


queryRegisteredSentences :: EngineWrapper -> RegisteredSentences -> IO BL.ByteString
queryRegisteredSentences engine r = do
  -- need to be configured.  
  let bstr = BL.toStrict $ encode (r { rs_max_clip_len = Just 200 })  
  bstr' <- B.useAsCString bstr $
     json_tparse >=> query engine >=> serialize >=> unsafePackCString
  return (BL.fromStrict bstr')

querySuggestion :: EngineWrapper -> [Text] -> IO BL.ByteString
querySuggestion engine ideas = do
  let bstr = BL.toStrict $ encode (object [ "ideas" .= ideas ])
  bstr' <- B.useAsCString bstr $
     json_tparse >=> suggest engine >=> serialize >=> unsafePackCString
  return (BL.fromStrict bstr')
  
type ResultBstr = BL.ByteString

failed :: BL.ByteString
failed = encode Null


queryWorker :: TMVar (HM.HashMap Text ([Int],[Text]))
            -> SendPort ResultBstr
            -> EngineWrapper
            -> Query
            -> Process ()
queryWorker resultref sc engine QueryText {..} = do
  m <- liftIO $ atomically $ takeTMVar resultref
  case HM.lookup query_text m of
    Just (ids,countries) -> do
      r <- liftIO (queryRegisteredSentences engine
                    RS { rs_sent_uids = ids
                       , rs_Countries = countries
                       , rs_confine_ygp_table_columns = query_tables
                       , rs_max_clip_len = Nothing })
      liftIO $ atomically $ putTMVar resultref m
      sendChan sc r
    Nothing -> do
      r' <- runMaybeT $ do
        liftIO $ putStrLn "before registerText"
        r <- registerText engine query_text
        liftIO $ print r 
        liftIO $ putStrLn "after registerText"
        resultbstr <- liftIO (queryRegisteredSentences engine 
                               r { rs_confine_ygp_table_columns = query_tables }
                             )
        return (r,resultbstr)
      case r' of
        Just (r,resultbstr) -> do
          liftIO $ atomically $ putTMVar resultref (HM.insert query_text (rs_sent_uids r,rs_Countries r) m)
          sendChan sc resultbstr
        Nothing         -> do
          liftIO $ atomically $ putTMVar resultref m
          sendChan sc failed
queryWorker resultref sc engine QueryRegister {..} = do
  m <- liftIO $ atomically $ takeTMVar resultref
  case HM.lookup query_register m of
    Just (ids,countries) -> do
      liftIO $ atomically $ putTMVar resultref m
      sendChan sc . encode $ RS { rs_sent_uids = ids
                                , rs_Countries = countries
                                , rs_confine_ygp_table_columns = []
                                , rs_max_clip_len = Nothing}
    Nothing -> do
      r' <- runMaybeT $ do
        r <- registerText engine query_register
        let resultbstr = encode r
        return (r,resultbstr)
      case r' of
        Just (r,resultbstr) -> do
          liftIO $ atomically $ putTMVar resultref (HM.insert query_register (rs_sent_uids r,rs_Countries r) m)
          sendChan sc resultbstr
        Nothing         -> do
          liftIO $ atomically $ putTMVar resultref m
          sendChan sc failed 
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
queryWorker resultref sc engine QuerySuggest {..} = do
  m <- liftIO $ atomically $ takeTMVar resultref
  resultbstr <- liftIO (querySuggestion engine query_ideas)
  liftIO $ atomically $ putTMVar resultref m
  sendChan sc resultbstr
