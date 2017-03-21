module CloudHaskell.Server where

import           Control.Concurrent                (forkIO)
import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Monad                     (void)
import qualified Data.HashMap.Strict         as HM
--
import           Broadcast
import           Network.Util

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
