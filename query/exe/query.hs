{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Concurrent                        (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class 
import           Control.Monad.Loops
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Maybe
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Distributed.Process.Serializable
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec                     as A
import qualified Data.Binary                         as Bi (encode,decode)
import qualified Data.ByteString.Base64.Lazy         as B64
import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Char8               as B

import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.ByteString.Unsafe                    (unsafeUseAsCStringLen,unsafePackCString)
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Map                            as M
import           Data.Maybe                                (maybeToList)
import           Data.Scientific

import           Data.Text                                 (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
import qualified Data.Text.Lazy.Encoding             as TLE
import qualified Data.Text.IO                        as TIO
import           Data.UUID                                 (toString)
import           Data.UUID.V4                              (nextRandom)
import           Foreign.C.String
import           Foreign.C.Types                           (CInt(..))
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Network.HTTP.Types                        (Method,methodGet,methodPost)
import           Network.Transport.ZMQ                     (createTransport, defaultZMQParameters)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO                                 (hClose, hGetContents, hPutStrLn, stderr)
import           System.Process                            (readProcess)
--
import           QueryServer.Type
import           CoreNLP
import           JsonUtil
import           Network

foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query"          c_query          :: Json_t -> IO Json_t
foreign import ccall "query_finalize" c_query_finalize :: IO ()



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

    
  
server :: String -> Process ()
server url = do
  str <- liftIO (BL.unpack <$> simpleHttpClient False methodGet url Nothing)
  runMaybeT $ do
    m <- (MaybeT . return) (Data.Aeson.decode (BL.pack str)) :: MaybeT Process (M.Map String String)
    pidstr <- (MaybeT . return) (M.lookup "result" m)
    liftIO $ hPutStrLn stderr (show pidstr)
    let them = (Bi.decode . B64.decodeLenient . BL.pack) pidstr
    lift $ do
      let heartbeat n = send them (HB n) >> liftIO (threadDelay 5000000) >> heartbeat (n+1)
      us <- getSelfPid
      (sc,rc) <- newChan :: Process (SendPort Query, ReceivePort Query)
      send them (sc,us)
      sc' <- expect :: Process (SendPort BL.ByteString)
      spawnLocal (heartbeat 0)

      ref <- liftIO $ newTVarIO HM.empty
      forever $ do
        q <- receiveChan rc
        liftIO $ hPutStrLn stderr (show q)
        spawnLocal (queryWorker ref sc' q)
  return ()


main = do
  serverurl <- liftIO (getEnv "SERVERURL")
  apilevel <- liftIO (getEnv "APILEVEL")
  let configurl = serverurl </> apilevel </> "config"
  
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  
  withCString "config.json" $ \configfile -> do
    c_query_init configfile
    runProcess node (server configurl)
    c_query_finalize

