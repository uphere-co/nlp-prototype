import           Control.Distributed.Process
import           Control.Distributed.Process.Node (initRemoteTable,newLocalNode,runProcess)
import qualified Data.Binary            as Bi (decode)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network.Transport.ZMQ (createTransport, defaultZMQParameters)

import           System.Environment

client :: Int -> Process ()
client msg = do
  pid <- getSelfPid
  them <- readProcessId
  liftIO $ print them
  send them msg

readProcessId :: Process ProcessId
readProcessId = liftIO $ Bi.decode <$> BL.readFile "server.pid"
  
main :: IO ()
main = do
  [host,msgstr] <- getArgs
  let msg = read msgstr :: Int
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  runProcess node (client msg)
