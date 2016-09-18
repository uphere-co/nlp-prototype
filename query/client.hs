import           Control.Distributed.Process
import           Control.Distributed.Process.Node (initRemoteTable,newLocalNode,runProcess)
import qualified Data.Binary            as Bi (decode)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.Transport.ZMQ (createTransport, defaultZMQParameters)

import           System.Environment
-- rtable :: RemoteTable
-- rtable = __remoteTable initRemoteTable

client :: Process ()
client = do
  pid <- getSelfPid
  them <- readProcessId
  liftIO $ print them
  -- (sc,rc) <- newChan :: Process (SendPort Int, ReceivePort Int)
  -- send them (pid,sc)
  send them ("abc" :: String)
  send them (100 :: Int)

readProcessId :: Process ProcessId
readProcessId = liftIO $ Bi.decode <$> BL.readFile "server.pid"
  

main :: IO ()
main = do
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  runProcess node client
