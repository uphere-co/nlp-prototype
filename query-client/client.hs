{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process.Lifted
import           Control.Distributed.Process.Node
import qualified Control.Exception                  as Ex
import           Control.Monad                            (void)
import           Control.Monad.Trans.Reader               (ask,runReaderT)
import qualified Data.ByteString.Lazy.Char8         as BL
import qualified Network.Simple.TCP                 as NS
import           Options.Applicative
--
import           CloudHaskell.Server
import           Network.Transport.UpHere       ( createTransport
                                                , defaultTCPParameters
                                                , DualHostPortPair(..))
import           Network.Util
import           QueryServer.Type
-- import           QueryQueue



data ClientOption = ClientOption { port :: Int
                                 , hostg :: String
                                 , hostl :: String
                                 , serverip :: String
                                 , serverport :: Int
                                 } deriving Show

pOptions :: Parser ClientOption
pOptions = ClientOption <$> option auto (long "port" <> short 'p' <> help "Port number")
                        <*> strOption (long "global-ip" <> short 'g' <> help "Global IP address")
                        <*> strOption (long "local-ip" <> short 'l' <> help "Local IP address")
                        <*> strOption (long "server-ip" <> short 's' <> help "Server IP address")
                        <*> option auto (long "server-port" <> short 'q' <> help "Server Port")

clientOption = info pOptions (fullDesc <> progDesc "Client")

initProcess :: {- QQVar -> -} ProcessId -> LogProcess ()
initProcess {- qqvar -} them = do
  us <- getSelfPid
  tellLog ("we are " ++ show us)
  send them us
  void (mainProcess them {-  qqvar -})

pingHeartBeat :: ProcessId -> ProcessId -> Int -> LogProcess ()
pingHeartBeat p1 them n = do
  send them (HB n)
  liftIO (threadDelay 5000000)
  mhb <- expectTimeout 10000000
  case mhb of
    Just (HB n') -> do
      tellLog ("ping-pong : " ++ show n')
      pingHeartBeat p1 them (n+1)
    Nothing -> do
      tellLog ("heartbeat failed!")
      kill p1 "heartbeat dead"



mainProcess :: ProcessId {- -> QQVar -} -> LogProcess ()
mainProcess them {- qqvar -} = do
  tellLog "mainProcess started"
  msc :: Maybe (SendPort (Query,SendPort BL.ByteString)) <- expectTimeout 5000000

 

  case msc of
    Nothing -> tellLog "cannot receive query port"
    Just sc -> do
      tellLog "connection stablished to query server"
      lock <- ask
      p1 <- spawnLocal $ do
        liftIO $ print "p1"
{-       p1 <- spawnLocal $ liftIO (garbageCollector lock qqvar) 
      p2 <- spawnLocal $ do
        forever $ do
          (i,q)  <- liftIO $ atomically $ do
                      qq <- readTVar qqvar
                      case next qq of
                        Nothing -> retry
                        Just (i,q) -> do
                          let qq' = IM.update (\_ -> Just (BeingProcessed q)) i qq
                          writeTVar qqvar qq'
                          return (i,q)
          tellLog ("query start: " ++ show (i,q))
          spawnLocal $ queryProcess qqvar sc (i,q)
      -}
      void $ pingHeartBeat p1 them 0   

{- 
queryProcess :: QQVar -> SendPort (Query, SendPort ResultBstr) -> (Int,Query) -> LogProcess ()
queryProcess qqvar sc (i,q) = do
  (sc',rc') <- newChan :: LogProcess (SendPort ResultBstr, ReceivePort ResultBstr)
  sendChan sc (q,sc')
  bstr <- receiveChan rc'
  bstr `seq` do
    tellLog (BL.unpack bstr)
    liftIO $ atomically $ 
      modifyTVar' qqvar (IM.update (\_ -> Just (Answered q bstr)) i)


retrieveQueryServerPid :: LogLock -> QueryConf -> IO (Maybe ProcessId)
retrieveQueryServerPid lock qconf = do
  let qserver = T.unpack (queryIP qconf)    -- "QUERYSERVERIP"
      port    = T.unpack (queryPort qconf)  -- "QUERYSERVERPORT"
  NS.connect qserver port $ \(sock,addr) -> do
    atomicLog lock ("connection established to " ++ show addr)
    recvAndUnpack sock
-}

retrieveQueryServerPid :: LogLock -> ClientOption -> IO (Maybe ProcessId)
retrieveQueryServerPid lock opt = do
  NS.connect (serverip opt) (show (serverport opt)) $ \(sock,addr) -> do
    atomicLog lock ("connection established to " ++ show addr)
    recvAndUnpack sock

main :: IO ()
main = do
  opt <- execParser clientOption
  putStrLn "client"
  print opt

  let dhpp = DHPP (hostg opt,show (port opt)) (hostl opt,show (port opt))
  Right transport <- createTransport dhpp defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  lock <- newLogLock 0
  emthem <- Ex.try (retrieveQueryServerPid lock opt)
  case emthem of
    Left (e :: Ex.SomeException) -> do
      atomicLog lock "exception caught"
      atomicLog lock (show e)
    Right mthem -> 
      case mthem of
        Nothing -> atomicLog lock "no pid"  
        Just them -> do
          atomicLog lock (show them)
          runProcess node (flip runReaderT lock (initProcess them))
