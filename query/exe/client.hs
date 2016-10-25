{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Distributed.Process
import           Control.Distributed.Process.Node       (initRemoteTable,newLocalNode,runProcess)
import qualified Data.Binary                      as Bi (decode)
import           Data.ByteString.Char8                  (ByteString)
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.Monoid                            ((<>))
import qualified Data.Text                        as T
import           Network.Transport.ZMQ                  (createTransport, defaultZMQParameters)
import           System.Environment
--
import           Type

client :: Maybe Query -> Process ()
client mmsgs = do
  pid <- getSelfPid
  them <- readProcessId
  liftIO $ print them
  (sc,rc) <- newChan :: Process (SendPort BL.ByteString, ReceivePort BL.ByteString)
  send them ((,) <$> mmsgs <*> pure sc)
  case mmsgs of
    Just _ -> do str <- receiveChan rc
                 liftIO $ BL.putStrLn ("message received: " <> str)
    Nothing -> return ()
  
readProcessId :: Process ProcessId
readProcessId = liftIO $ Bi.decode <$> BL.readFile "server.pid"
  
main :: IO ()
main = do
  (host:msgs) <- getArgs
  let mmsgs = if null msgs then Nothing else Just (Query (map T.pack msgs))
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  runProcess node (client mmsgs)
