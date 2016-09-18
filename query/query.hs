{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad
import           Control.Monad.IO.Class 
import           Control.Monad.Loops
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import qualified Data.Binary                         as Bi (encode)
import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Char8               as B
import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.Text                                 (Text)
import qualified Data.Text                           as T
import           Data.UUID                                 (toString)
import           Data.UUID.V4                              (nextRandom)
import           Foreign.C.String
import           Network.Transport.ZMQ                     (createTransport, defaultZMQParameters)
import           System.Directory
import           System.Environment
import           System.FilePath
--
import           Type



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

server :: {- CString -> -} Process ()
server = do
  writeProcessId
  whileJust_ expect $ \q -> do
    (mapM_ (liftIO . putStrLn) .  querySentences) q 
    withTempFile $ \fp -> liftIO $ do
      writeFile fp (makeJsonString q)
      withCString fp $ \queryfile -> liftIO $ c_query queryfile
      removeFile fp

withTempFile :: (MonadIO m) => (FilePath -> m a) -> m a
withTempFile f = do
  tmp <- liftIO getTemporaryDirectory
  uuid <- liftIO nextRandom
  liftIO $ print uuid
  let tmpfile = tmp </> (toString uuid) <.> "json"
  f tmpfile
  -- removeFile tmpfile

makeJsonString :: Query -> String
makeJsonString (Query qs) = "{ \"phrase_store\" : \"/data/groups/uphere/parsers/rnn_model4/phrases.h5\", \"phrase_vec\" : \"news_wsj.text.vecs\", \"phrase_word\" : \"news_wsj.test.words\", \"rnn_param_store\" : \"/data/groups/uphere/data/groups/uphere/parsers/rnn_model4/rnn_params.h5\",\"rnn_param_uid\" : \"model4.d877053.2000\",\"wordvec_store\" : \"/data/groups/uphere/parsers/rnn_model4/news_wsj.h5\",\"voca_name\" : \"news_wsj.voca\",\"w2vmodel_name\" : \"news_wsj\",\"queries\" : " ++ show qs ++ " }"

main = do
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  
  withCString "config.json" $ \configfile -> do
    c_query_init configfile
    runProcess node server
    c_query_finalize
