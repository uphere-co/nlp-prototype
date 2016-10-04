{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Concurrent                        (forkIO, threadDelay)
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.IO.Class 
import           Control.Monad.Loops
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Data.Aeson
import qualified Data.Binary                         as Bi (encode)
import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Char8               as B
import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.ByteString.Unsafe                    (unsafeUseAsCStringLen,unsafePackCString)
import           Data.Text                                 (Text)
import qualified Data.Text                           as T
import           Data.UUID                                 (toString)
import           Data.UUID.V4                              (nextRandom)
import           Foreign.C.String
import           Foreign.C.Types                           (CInt(..))
import           Foreign.Ptr
import           Network.Transport.ZMQ                     (createTransport, defaultZMQParameters)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO                                 (hClose, hGetContents, hPutStrLn)
--
import           Type
import           Util.Json


--foreign import ccall "create_unique_ptr" c_create_unique_ptr :: IO (Ptr ())

foreign import ccall "make_input"     c_make_input     :: CString -> IO Json_t
foreign import ccall "process"        c_process        :: Json_t -> IO ()
foreign import ccall "myclose"        c_close          :: Json_t -> IO ()

foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query"          c_query          :: Json_t -> IO Json_t
foreign import ccall "get_output"     c_get_output     :: Json_t -> IO CString
foreign import ccall "query_finalize" c_query_finalize :: IO ()

writeProcessId :: Process ()
writeProcessId = do
  us <- getSelfPid
  liftIO $ print us
  liftIO $ BL.writeFile "server.pid" (Bi.encode us)

queryWorker :: SendPort BL.ByteString -> Query -> Process ()
queryWorker sc q = do
  let r = encode (makeJson q)
      bstr = BL.toStrict r 
  bstr' <- liftIO $ B.useAsCString bstr $ \cstr -> do
    p <- c_make_input cstr
    c_process p
    c_close p 



    -- >>= c_query >>= c_get_output >>= unsafePackCString
  -- sendChan sc (BL.fromStrict bstr')
  return ()
  
server :: Process ()
server = do
  writeProcessId
  whileJust_ expect $ \(q,sc) -> spawnLocal (queryWorker sc q)

  {- 
withTempFile :: (MonadIO m) => (FilePath -> m a) -> m a
withTempFile f = do
  tmp <- liftIO getTemporaryDirectory
  uuid <- liftIO nextRandom
  liftIO $ print uuid
  let tmpfile = tmp </> (toString uuid) <.> "json"
  f tmpfile
-}

makeJson :: Query -> Value
makeJson (Query qs) = object [ "queries" .= toJSON qs ]

main = do
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  
  withCString "config.json" $ \configfile -> do
    c_query_init configfile
    runProcess node server
    c_query_finalize
