{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Distributed.Process.Node
import qualified Control.Exception                  as Ex
import           Control.Monad.Trans.Reader               (runReaderT)
import           Options.Applicative
--
import           Network.Transport.UpHere       ( createTransport
                                                , defaultTCPParameters
                                                , DualHostPortPair(..))
import           Network.Util
--
import           Client
import           Type




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
