module Broadcast where

import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar, takeTMVar)
import           Control.Distributed.Process       (ProcessId)
import           Network.Simple.TCP                (serve,HostPreference(..))
--
import           Network.Util

broadcastProcessId :: TMVar ProcessId -> String -> IO ()
broadcastProcessId pidref port = do
  serve HostAny port $ \(sock,addr) -> do
    putStrLn $ "TCP connection established from " ++ show addr
    pid <- atomically (takeTMVar pidref) 
    packAndSend sock pid
