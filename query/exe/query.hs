{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent                        (forkIO,threadDelay)
import           Control.Concurrent.STM
import           Control.Exception                         (throwIO)
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
import           Foreign.C.String
import           Network.HTTP.Types                        (methodGet)
import           Network.Transport                         (Transport(..))
import           Network.Transport.UpHere    (createTransport,defaultTCPParameters
                                             ,DualHostPortPair(..))
import           System.Environment
import           System.FilePath
import           System.IO                                 (hPutStrLn, stderr)
--
import           Query.Binding.EngineWrapper
import           Query.Binding.Json_t
import           QueryServer.Type
--
import           Broadcast
import           Network
import           Worker

withHeartBeat :: ProcessId -> Process ProcessId -> Process ()
withHeartBeat them action = do
  pid <- action                                            -- main process launch
  whileJust_ (expectTimeout 10000000) $ \(HB n) -> do      -- heartbeating until it fails. 
    liftIO $ hPutStrLn stderr ("heartbeat: " ++ show n)
    send them (HB n)
      
  liftIO $ hPutStrLn stderr "heartbeat failed: reload"     -- when fail, it prints messages  
  kill pid "connection closed"                             -- and start over the whole process.

  
server :: String -> EngineWrapper -> Process ()
server port engine = do
  pid <- getSelfPid
  liftIO $ hPutStrLn stderr (show pid)
  
  void . liftIO $ forkIO (broadcastProcessId pid port)
  liftIO $ putStrLn "server started"
  let go = do
        mthem <- expectTimeout 10000000
        case mthem of
          Nothing -> liftIO $ hPutStrLn stderr "cannot get client pid"
          Just them -> do
            liftIO $ hPutStrLn stderr ("got client pid : " ++ show them)
            withHeartBeat them $ spawnLocal $ do
              (sc,rc) <- newChan :: Process (SendPort (Query, SendPort ResultBstr), ReceivePort (Query, SendPort ResultBstr))
              send them sc
              liftIO $ hPutStrLn stderr "connected"  
              ref <- liftIO $ newTVarIO HM.empty
              forever $ do
                (q,sc') <- receiveChan rc
                liftIO $ hPutStrLn stderr (show q)
                spawnLocal (queryWorker ref sc' engine q)
  forever $ go

  
main :: IO ()
main = do
  port <- getEnv "PORT"
  let portnum :: Int = read port
      port' = show (portnum+1)
  [hostg,hostl,config] <- getArgs
  let dhpp = DHPP (hostg,port') (hostl,port')
  etransport <- createTransport dhpp defaultTCPParameters
  case etransport of
    Left err -> hPutStrLn stderr (show err)
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      withCString config $ \configfile -> do
        engine <- newEngineWrapper configfile
        runProcess node (server port engine)
        deleteEngineWrapper engine


