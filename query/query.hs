{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node      (initRemoteTable,newLocalNode,runProcess)
import qualified Data.Binary            as Bi (encode)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text                   (Text)
import qualified Data.Text              as T
import           Foreign.C.String
import           Network.Transport.ZMQ       (createTransport, defaultZMQParameters)
import           System.Environment

foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query"          c_query          :: CString -> IO ()
foreign import ccall "query_finalize" c_query_finalize :: IO ()

--rtable :: RemoteTable
-- rtable = __remoteTable initRemoteTable


writeProcessId :: Process ()
writeProcessId = do
  us <- getSelfPid
  liftIO $ print us
  liftIO $ BL.writeFile "server.pid" (Bi.encode us)

server :: Process ()
server = do
  writeProcessId
  -- (theirpid :: ProcessId, them) <- expect
  -- say $ "establish connection to " ++ show theirpid
  forever $ do
    n :: Int <- expect
    liftIO $ print n
    return ()


main = do
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  runProcess node server

  {- 

initialClient 
  
  withCString "config.json" $ \configfile -> do
    withCString "/data/groups/uphere/similarity_test/input.json" $ \queryfile -> do
      c_query_init configfile
      c_query queryfile
      c_query_finalize


-}
