{-# LANGUAGE TemplateHaskell #-}

import Control.Distributed.Process
import Control.Distributed.Process.Async
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node                   (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Environment                                 (getArgs)
--
-- import Common
  
dummy :: () -> Process ()
dummy () = do
  liftIO $ putStrLn "dummy"
  
remotable ['dummy]



master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  liftIO $ putStrLn $ "slaves: " ++ show slaves
  let tasks = map (\s-> AsyncRemoteTask $(functionTDict 'dummy) s ($(mkClosure 'dummy) ())) slaves
  as <- mapM async tasks

  rs <- mapM wait as
  -- liftIO $ print rs 
  terminateAllSlaves backend


  


main :: IO ()
main = do
  putStrLn "simple local net test"
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port (__remoteTable initRemoteTable)
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port (__remoteTable initRemoteTable)
      startSlave backend
