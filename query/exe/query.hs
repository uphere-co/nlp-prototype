{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                        (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class 
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Maybe
import           Control.Distributed.Process
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Data.Aeson
import qualified Data.Binary                         as Bi (decode,encode)
import qualified Data.ByteString.Base64.Lazy         as B64
import qualified Data.ByteString.Char8               as B
import qualified Data.ByteString.Lazy.Char8          as BL
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Map                            as M
import qualified Database.Redis                      as DR
import           Foreign.C.String
import           Network.HTTP.Types                        (methodGet)
import           Network.Transport.ZMQ                     (createTransport, defaultZMQParameters)
import           System.Environment
import           System.FilePath
import           System.IO                                 (hPutStrLn, stderr)
--
import           QueryServer.Type
import           Network
import           Worker

foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query_finalize" c_query_finalize :: IO ()


writeProcessId :: Process ()
writeProcessId = do
  usb64 <- BL.toStrict . B64.encode . Bi.encode <$> getSelfPid
  let cinfo = DR.defaultConnectInfo
              { DR.connectHost = "localhost"
              , DR.connectPort = DR.PortNumber 6379 }
  conn <- liftIO $ DR.connect cinfo
  void . liftIO $ DR.runRedis conn $ DR.set "query.devel.pid" usb64
  


server :: String -> Process ()
server url = do
  writeProcessId
  them :: ProcessId <- expect

  let heartbeat n = send them (HB n) >> liftIO (threadDelay 5000000) >> heartbeat (n+1)
  -- us <- getSelfPid
  (sc,rc) <- newChan :: Process (SendPort (Query, SendPort ResultBstr), ReceivePort (Query, SendPort ResultBstr))
  send them sc
  liftIO $ putStrLn "connected"  
  spawnLocal (heartbeat 0)
  ref <- liftIO $ newTVarIO HM.empty
  forever $ do
    (q,sc') <- receiveChan rc
    liftIO $ hPutStrLn stderr (show q)
    spawnLocal (queryWorker ref sc' q)


{-  
  str <- liftIO (BL.unpack <$> simpleHttpClient False methodGet url Nothing)
  runMaybeT $ do
    m <- (MaybeT . return) (Data.Aeson.decode (BL.pack str)) :: MaybeT Process (M.Map String String)
    pidstr <- (MaybeT . return) (M.lookup "result" m)
    liftIO $ hPutStrLn stderr (show pidstr)
    let them = (Bi.decode . B64.decodeLenient . BL.pack) pidstr
-}


main :: IO ()
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

