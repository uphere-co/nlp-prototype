{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops                       (whileJust_)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import           Foreign.C.String
import           Network.Transport.UpHere    (createTransport,defaultTCPParameters
                                             ,DualHostPortPair(..))
import           Options.Applicative
import           System.Environment
import           System.IO                                 (hPutStrLn, stderr)
--
import           Query.Binding.EngineWrapper
import           QueryServer.Type
--
import           CloudHaskell.Server
import           Network.Util
import           Worker

start :: EngineWrapper -> TMVar (HM.HashMap Text ([Int],[Text])) -> LogLock -> Process () 
start engine resultref lock = do
  them :: ProcessId <- expect
  atomicLog lock ("got client pid : " ++ show them)
  withHeartBeat lock them $ spawnLocal $ do
    (sc,rc) <- newChan :: Process (SendPort (Query, SendPort ResultBstr), ReceivePort (Query, SendPort ResultBstr))
    send them sc
    liftIO $ hPutStrLn stderr "connected"  
    forever $ do
      (q,sc') <- receiveChan rc
      liftIO $ hPutStrLn stderr (show q)
      spawnLocal (queryWorker resultref sc' engine q)


data ServerOption = ServerOption { _port :: Int
                                 , _hostg :: String
                                 , _hostl :: String
                                 , _config :: String
                                 }

pOptions :: Parser ServerOption
pOptions = ServerOption <$> option auto (long "port" <> short 'p' <> help "Port number")
                        <*> strOption (long "global-ip" <> short 'g' <> help "Global IP address")
                        <*> strOption (long "local-ip"  <> short 'l' <> help "Local IP address")
                        <*> strOption (long "config-file" <> short 'c' <> help "Config file")


queryServerOption = info pOptions ( fullDesc <> progDesc "Query server daemon" <> header "options are port, global-ip, local-ip")

  
main :: IO ()
main = do
  {- 
  port <- getEnv "PORT"
  let portnum :: Int = read port
      port' = show (portnum+1)
  [hostg,hostl,config] <- getArgs
  let dhpp = DHPP (hostg,port') (hostl,port') -}
  opt <- execParser queryServerOption

  let portnum = _port opt 
      port = show portnum 
      port' = show (portnum+1)
      hostg = _hostg opt
      hostl = _hostl opt
      config = _config opt
      dhpp = DHPP (hostg,port') (hostl,port')
  
  etransport <- createTransport dhpp defaultTCPParameters
  case etransport of
    Left err -> hPutStrLn stderr (show err)
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      withCString config $ \configfile -> do
        engine <- newEngineWrapper configfile
        runProcess node (server port start  engine)
        deleteEngineWrapper engine


