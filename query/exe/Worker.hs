{-# LANGUAGE ForeignFunctionInterface #-}

module Worker where

import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Distributed.Process.Serializable
import           Control.Monad
import           Data.Aeson

import qualified Data.ByteString.Char8               as B

import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.ByteString.Unsafe                    (unsafeUseAsCStringLen,unsafePackCString)

import qualified Data.HashMap.Strict                 as HM
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
import           Foreign.ForeignPtr

--
import           QueryServer.Type
import           CoreNLP
import           JsonUtil

foreign import ccall "query"          c_query          :: Json_t -> IO Json_t

query :: Json -> IO Json
query q = withForeignPtr q c_query >>= newForeignPtr c_json_finalize


queryWorker :: TVar (HM.HashMap Query BL.ByteString) -> SendPort BL.ByteString -> Query -> Process ()
queryWorker ref sc q = do
  m <- liftIO $ readTVarIO ref
  case HM.lookup q m of
    Nothing -> do
      let failed =encode Null
          ss = querySentences q
      case ss of
        (s:_) -> do
          if (not . T.null) s
            then do
              bstr <- (liftIO . runCoreNLP . TE.encodeUtf8) s
              bstr' <- liftIO $ B.useAsCString bstr $ 
                json_create >=> query >=> json_serialize >=> unsafePackCString
              let resultbstr = BL.fromStrict bstr'
              liftIO $ atomically (modifyTVar' ref (HM.insert q resultbstr))
              sendChan sc resultbstr
            else 
              sendChan sc failed 
        [] -> sendChan sc failed
    Just resultbstr -> sendChan sc resultbstr

