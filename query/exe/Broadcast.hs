module Broadcast where

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

broadcastProcessId :: ProcessId -> IO ()
broadcastProcessId pid = do
  serve HostAny "5002" $ \(sock,addr) -> do
    putStrLn $ "TCP connection established from " ++ show addr
    packAndSend sock pid
