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

type LogLock = (TMVar (),Int)

atomicLog lock str = liftIO $ do
  let n = snd lock
  atomically $ takeTMVar (fst lock)
  hPutStrLn stderr ("[" ++ show n ++ "]: " ++ str)
  atomically $ putTMVar (fst lock) ()

getClientNum (l,n) = n
incClientNum (l,n) = (l,n+1)

withHeartBeat :: LogLock -> ProcessId -> Process ProcessId -> Process ()
withHeartBeat lock them action = do
  pid <- action                                            -- main process launch
  whileJust_ (expectTimeout 10000000) $ \(HB n) -> do      -- heartbeating until it fails. 
    atomicLog lock ("heartbeat: " ++ show n)
    send them (HB n)
      
  atomicLog lock "heartbeat failed: reload"     -- when fail, it prints messages  
  kill pid "connection closed"                             -- and start over the whole process.

  
server :: String -> EngineWrapper -> Process ()
server port engine = do
  pidref <- liftIO newEmptyTMVarIO 
  void . liftIO $ forkIO (broadcastProcessId pidref port)
  liftIO $ putStrLn "server started"
  resultref <- liftIO $ newTMVarIO HM.empty
  lock <- (,) <$> liftIO (newTMVarIO ()) <*> pure 0
  
  serve lock pidref (start engine resultref)



serve lock pidref action = do
  atomicLog lock ("waiting a new client")
  pid <- spawnLocal (action lock)
  atomicLog lock (show pid)
  liftIO (atomically (putTMVar pidref pid))
  serve (incClientNum lock) pidref action


start engine resultref lock = do
  them :: ProcessId <- expect
  atomicLog lock ("got client pid : " ++ show them)
  withHeartBeat lock them $ spawnLocal $ do
    (sc,rc) <- newChan :: Process (SendPort (Query, SendPort ResultBstr), ReceivePort (Query, SendPort ResultBstr))
    send them sc
    liftIO $ hPutStrLn stderr "connected"  
    forever $ do
      (q,sc') <- receiveChan rc
      liftIO $ hPutStrLn stderr (show q)
      spawnLocal (queryWorker resultref sc' engine q)
 
  
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


