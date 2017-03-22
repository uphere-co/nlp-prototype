module CloudHaskell.Server where

import           Control.Concurrent                (forkIO)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar, takeTMVar, newTMVarIO, newEmptyTMVarIO, putTMVar)
import           Control.Distributed.Process       (ProcessId, Process, expectTimeout, kill, send, spawnLocal)
import           Control.Monad                     (void)
import           Control.Monad.Loops               (whileJust_)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.Binary                 as Bi
import qualified Data.HashMap.Strict         as HM
import qualified Network.Simple.TCP          as NS
--
import           Network.Util

data HeartBeat = HB { heartBeat :: Int }

instance Bi.Binary HeartBeat where
  put (HB n) = Bi.put n
  get = HB <$> Bi.get
  
withHeartBeat :: LogLock -> ProcessId -> Process ProcessId -> Process ()
withHeartBeat lock them action = do
  pid <- action                                            -- main process launch
  whileJust_ (expectTimeout 10000000) $ \(HB n) -> do      -- heartbeating until it fails. 
    atomicLog lock ("heartbeat: " ++ show n)
    send them (HB n)
      
  atomicLog lock "heartbeat failed: reload"     -- when fail, it prints messages  
  kill pid "connection closed"                             -- and start over the whole process.



broadcastProcessId :: LogLock -> TMVar ProcessId -> String -> IO ()
broadcastProcessId lock pidref port = do
  NS.serve NS.HostAny port $ \(sock,addr) -> do
    atomicLog lock ("TCP connection established from " ++ show addr)
    pid <- atomically (takeTMVar pidref) 
    packAndSend sock pid


serve :: LogLock -> TMVar ProcessId -> (LogLock -> Process ()) -> Process ()
serve lock pidref action = do
  atomicLog lock ("waiting a new client!")
  pid <- spawnLocal (action lock)
  atomicLog lock (show pid)
  liftIO (atomically (putTMVar pidref pid))
  serve (incClientNum lock) pidref action


server :: String -> (p -> TMVar (HM.HashMap k v)  -> LogLock -> Process ()) -> p -> Process ()
server port action p = do
  pidref <- liftIO newEmptyTMVarIO
  liftIO $ putStrLn "server started"
  resultref <- liftIO $ newTMVarIO HM.empty
  lock <- newLogLock 0 
  
  void . liftIO $ forkIO (broadcastProcessId lock pidref port)
  serve lock pidref (action p resultref)
