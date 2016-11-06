{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Worker where

import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8               as B
import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.ByteString.Unsafe                    (unsafePackCString)
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
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


queryWorker :: TVar (HM.HashMap Query BL.ByteString) -> SendPort BL.ByteString -> Query -> Process ()
queryWorker ref sc q = do
  m <- liftIO $ readTVarIO ref
  case HM.lookup q m of
    Nothing -> do
      case querySentences q of
        (s:_) -> do
          if (not . T.null) s
            then do
              bstr <- (liftIO . runCoreNLP . TE.encodeUtf8) s
              bstr0 <- liftIO $ B.useAsCString bstr $
                json_create >=> register_documents >=> json_serialize >=> unsafePackCString
              liftIO $ B.putStrLn bstr0
              let r :: Either String RegisteredSentences  = eitherDecodeStrict' bstr0
              liftIO $ print r
              bstr' <- liftIO $ B.useAsCString bstr $ 
                json_create >=> query >=> json_serialize >=> unsafePackCString
              let resultbstr = BL.fromStrict bstr'
              liftIO $ atomically (modifyTVar' ref (HM.insert q resultbstr))
              sendChan sc resultbstr
            else 
              sendChan sc failed 
        [] -> sendChan sc failed
    Just resultbstr -> sendChan sc resultbstr

