module Broadcast where

import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar, takeTMVar)
import           Control.Distributed.Process       (ProcessId)
import           Data.Binary
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import           Network.Simple.TCP                        

packNumBytes :: B.ByteString -> B.ByteString
packNumBytes bstr =
  let len = (fromIntegral . B.length) bstr :: Word32
  in BL.toStrict (encode len)

packAndSend :: (Binary a) => Socket -> a -> IO ()
packAndSend sock x = do
  let msg = (BL.toStrict . encode) x
      sizebstr = packNumBytes msg
  send sock sizebstr
  send sock msg

broadcastProcessId :: TMVar ProcessId -> String -> IO ()
broadcastProcessId pidref port = do
  serve HostAny port $ \(sock,addr) -> do
    putStrLn $ "TCP connection established from " ++ show addr
    pid <- atomically (takeTMVar pidref) 
    packAndSend sock pid
