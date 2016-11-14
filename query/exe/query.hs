{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                        (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops                       (whileJust_)
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


writeProcessId :: String -> String -> Process ()
writeProcessId redisip apilevel = do
  usb64 <- BL.toStrict . B64.encode . Bi.encode <$> getSelfPid

  (liftIO . print . B64.decodeLenient . BL.fromStrict) usb64

  let us :: ProcessId = (Bi.decode . B64.decodeLenient . BL.fromStrict) usb64
  liftIO $ print us
  
  let cinfo = DR.defaultConnectInfo
              { DR.connectHost = redisip
              , DR.connectPort = DR.PortNumber 6379 }
  conn <- liftIO $ DR.connect cinfo
  void . liftIO $ DR.runRedis conn $ DR.set (B.pack ("query." ++ apilevel ++ ".pid")) usb64
  

-- heartBeat n = do

withHeartBeat :: {- ProcessId -> -} Process ProcessId -> Process ()
withHeartBeat {- them -} action = do
  forever $ do                                               -- forever looping
    pid <- action                                            -- main process launch
    whileJust_ (expectTimeout 10000000) $ \(HB n) -> do      -- heartbeating until it fails. 
      liftIO $ hPutStrLn stderr ("heartbeat: " ++ show n)
      -- send them (HB n)
      
    liftIO $ hPutStrLn stderr "heartbeat failed: reload"     -- when fail, it prints messages  
    kill pid "connection closed"                             -- and start over the whole process.

  
server :: String -> String -> Process ()
server serverip apilevel = do
  writeProcessId serverip apilevel
  them :: ProcessId <- expect
  withHeartBeat $ spawnLocal $ do
    (sc,rc) <- newChan :: Process (SendPort (Query, SendPort ResultBstr), ReceivePort (Query, SendPort ResultBstr))
    send them sc
    liftIO $ putStrLn "connected"  
    -- spawnLocal (heartBeat 0)
    ref <- liftIO $ newTVarIO HM.empty
    forever $ do
      (q,sc') <- receiveChan rc
      liftIO $ hPutStrLn stderr (show q)
      spawnLocal (queryWorker ref sc' q)

main :: IO ()
main = do
  redisip <- liftIO (getEnv "REDISIP")
  apilevel <- liftIO (getEnv "APILEVEL")
  
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  
  withCString "config.json" $ \configfile -> do
    c_query_init configfile
    runProcess node (server redisip apilevel)
    c_query_finalize

