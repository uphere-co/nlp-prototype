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
import           Data.Conduit                              (runConduit, ($$))
import           Data.Conduit.Binary                       (sourceLbs,sourceHandle,sinkLbs,sinkHandle) 
import           Data.Text                                 (Text)
import qualified Data.Text                           as T
import           Data.UUID                                 (toString)
import           Data.UUID.V4                              (nextRandom)
import           Foreign.C.String
import           Foreign.C.Types                           (CInt(..))
import           Network.Transport.ZMQ                     (createTransport, defaultZMQParameters)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO                                 (hClose, hGetContents, hPutStrLn)
import           System.Posix.IO                           (closeFd, createPipe, fdToHandle, fdWrite)
import           System.Posix.Types                        (Fd(..))
--
import           Type

foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query"          c_query          :: Fd -> Fd -> IO () -- :: CString -> IO ()
foreign import ccall "query_finalize" c_query_finalize :: IO ()


data PipeDuplex = PipeDuplex { hereToThere :: (Fd,Fd)
                             , thereToHere :: (Fd,Fd) }


writeProcessId :: Process ()
writeProcessId = do
  us <- getSelfPid
  liftIO $ print us
  liftIO $ BL.writeFile "server.pid" (Bi.encode us)

pipeTransmit :: PipeDuplex -> BL.ByteString -> IO BL.ByteString
pipeTransmit (PipeDuplex (foq,fiq) (for,fir)) bstr = do
  hq <- fdToHandle fiq
  hr <- fdToHandle for
  runConduit $ sourceLbs bstr $$ sinkHandle hq
  runConduit $ sourceHandle hr $$ sinkLbs

mkDuplex :: IO PipeDuplex
mkDuplex = PipeDuplex <$> createPipe <*> createPipe

queryWorker :: SendPort BL.ByteString -> Query -> Process ()
queryWorker sc q = do
  duplex <- liftIO mkDuplex
  liftIO $ forkIO $ c_query ((fst.hereToThere) duplex) ((snd.thereToHere) duplex)
  liftIO (pipeTransmit duplex (encode (makeJson q))) >>= sendChan sc


server :: Process ()
server = do
  writeProcessId
  whileJust_ expect $ \(q,sc) -> spawnLocal (queryWorker sc q)
  
withTempFile :: (MonadIO m) => (FilePath -> m a) -> m a
withTempFile f = do
  tmp <- liftIO getTemporaryDirectory
  uuid <- liftIO nextRandom
  liftIO $ print uuid
  let tmpfile = tmp </> (toString uuid) <.> "json"
  f tmpfile

makeJson :: Query -> Value
makeJson (Query qs) =
  object [ "phrase_store"    .= ("/data/groups/uphere/parsers/rnn_model4/phrases.h5" :: Text)
         , "phrase_vec"      .= ("news_wsj.text.vecs" :: Text)
         , "phrase_word"     .= ("news_wsj.test.words" :: Text)
         , "rnn_param_store" .= ("/data/groups/uphere/data/groups/uphere/parsers/rnn_model4/rnn_params.h5" :: Text)
         , "rnn_param_uid"   .= ("model4.d877053.2000" :: Text)
         , "wordvec_store"   .= ("/data/groups/uphere/parsers/rnn_model4/news_wsj.h5" :: Text)
         , "voca_name"       .= ("news_wsj.voca" :: Text)
         , "w2vmodel_name"   .= ("news_wsj" :: Text)
         , "queries"         .= toJSON qs
         ] 

main = do
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  
  withCString "config.json" $ \configfile -> do
    c_query_init configfile
    runProcess node server
    c_query_finalize
