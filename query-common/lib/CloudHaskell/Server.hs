module CloudHaskell.Server where

import           Control.Concurrent                (forkIO)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar, takeTMVar, newTMVarIO, newEmptyTMVarIO, putTMVar)
import           Control.Distributed.Process       (ProcessId, Process, spawnLocal)
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.HashMap.Strict         as HM
import qualified Network.Simple.TCP          as NS
--
import           Network.Util


broadcastProcessId :: TMVar ProcessId -> String -> IO ()
broadcastProcessId pidref port = do
  NS.serve NS.HostAny port $ \(sock,addr) -> do
    putStrLn $ "TCP connection established from " ++ show addr
    pid <- atomically (takeTMVar pidref) 
    packAndSend sock pid


serve :: LogLock -> TMVar ProcessId -> (LogLock -> Process ()) -> Process ()
serve lock pidref action = do
  atomicLog lock ("waiting a new client")
  pid <- spawnLocal (action lock)
  atomicLog lock (show pid)
  liftIO (atomically (putTMVar pidref pid))
  serve (incClientNum lock) pidref action


server :: String -> (p -> TMVar (HM.HashMap k v)  -> LogLock -> Process ()) -> p -> Process ()
server port action p = do
  pidref <- liftIO newEmptyTMVarIO 
  void . liftIO $ forkIO (broadcastProcessId pidref port)
  liftIO $ putStrLn "server started"
  resultref <- liftIO $ newTMVarIO HM.empty
  lock <- newLogLock 0 
  serve lock pidref (action p resultref)
