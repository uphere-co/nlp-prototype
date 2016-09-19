{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           System.IO                                 (hGetContents)
import           System.Posix.IO                           (createPipe, fdToHandle)
import           System.Posix.Types                        (Fd(..))
--
import           Type

foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query"          c_query          :: CInt  -> IO () -- :: CString -> IO ()
foreign import ccall "query_finalize" c_query_finalize :: IO ()

writeProcessId :: Process ()
writeProcessId = do
  us <- getSelfPid
  liftIO $ print us
  liftIO $ BL.writeFile "server.pid" (Bi.encode us)


queryWorker :: Query -> IO ()
queryWorker q = do
  (fin,fout) <- createPipe
  let Fd fd_out = fout
  h <- fdToHandle fin
  
  c_query (fd_out :: CInt) -- fin

  str <- hGetContents h
  putStrLn "haskell side:"
  putStrLn str
  -- withTempFile $ \fp -> liftIO $ do
   --  BL.writeFile fp (encode (makeJson q))
    -- withCString fp $ \queryfile -> liftIO $ c_query queryfile
    -- removeFile fp


server :: Process ()
server = do
  writeProcessId
{-  
  forkIO $ do
    threadDelay 10000
    qs <- read var
    let enumqs = zip [1..] qs
    when ((not.null) qs) $ queryWorker
    
    rs' <- getresult
    zip qs rs' 
    flush var 
-}  
  whileJust_ expect $ \q -> do
    (mapM_ (liftIO . putStrLn) .  querySentences) q
    liftIO $ queryWorker q
    -- save var q 



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
