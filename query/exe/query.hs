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
--                                                           ,createBareLocalNode
--                                                           ,startServiceProcesses
--                                                           )
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
import           Network.Transport.ZMQ                     (apiNewEndPoint, createTransport, createTransportExposeInternals, defaultZMQParameters, Hints(..))
import           Network.Transport.ZMQ.Internal.Types      (TransportInternals(..))
import           System.Environment
import           System.FilePath
import           System.IO                                 (hPutStrLn, stderr)
--
import           QueryServer.Type
--
import           Broadcast
import           Network
import           Worker

foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query_finalize" c_query_finalize :: IO ()

withHeartBeat :: ProcessId -> Process ProcessId -> Process ()
withHeartBeat them action = do
  forever $ do                                               -- forever looping
    pid <- action                                            -- main process launch
    whileJust_ (expectTimeout 10000000) $ \(HB n) -> do      -- heartbeating until it fails. 
      liftIO $ hPutStrLn stderr ("heartbeat: " ++ show n)
      send them (HB n)
      
    liftIO $ hPutStrLn stderr "heartbeat failed: reload"     -- when fail, it prints messages  
    kill pid "connection closed"                             -- and start over the whole process.

  
server :: String -> Process ()
server port = do
  pid <- getSelfPid
  liftIO $ hPutStrLn stderr (show pid)
  
  void . liftIO $ forkIO (broadcastProcessId pid port)
  liftIO $ putStrLn "server started"
  them <- expect
  withHeartBeat them $ spawnLocal $ do
    (sc,rc) <- newChan :: Process (SendPort (Query, SendPort ResultBstr), ReceivePort (Query, SendPort ResultBstr))
    send them sc
    liftIO $ hPutStrLn stderr "connected"  
    ref <- liftIO $ newTVarIO HM.empty
    forever $ do
      (q,sc') <- receiveChan rc
      liftIO $ hPutStrLn stderr (show q)
      spawnLocal (queryWorker ref sc' q)

main :: IO ()
main = do
  port <- getEnv "PORT"
  let portnum = read port
      portnum' = portnum+1
  [host, config] <- getArgs
  (internals,transport) <- createTransportExposeInternals defaultZMQParameters (B.pack host)
  let transport' = transport { newEndPoint = apiNewEndPoint Hints { hintsPort = Just portnum'} internals }
  node <- newLocalNode transport' initRemoteTable
  
  withCString config $ \configfile -> do
    c_query_init configfile
    runProcess node (server port)
    c_query_finalize

