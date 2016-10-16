{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Concurrent                        (forkIO, threadDelay)
import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.IO.Class 
import           Control.Monad.Loops
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Maybe
import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Distributed.Process.Serializable
import           Data.Aeson
import           Data.Aeson.Encode                         (encodeToBuilder)
import qualified Data.Attoparsec                     as A
import qualified Data.Binary                         as Bi (encode,decode)
import qualified Data.Binary.Builder                 as B  (toLazyByteString)
import qualified Data.ByteString.Base64.Lazy         as B64
import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Char8               as B

import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.ByteString.Unsafe                    (unsafeUseAsCStringLen,unsafePackCString)
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Map                         as M

import           Data.Text                                 (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
import qualified Data.Text.Lazy.Encoding             as TLE
import qualified Data.Text.IO                        as TIO
import           Data.UUID                                 (toString)
import           Data.UUID.V4                              (nextRandom)
import qualified Data.Vector                         as V
import           Foreign.C.String
import           Foreign.C.Types                           (CInt(..))
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Network.Connection                        (TLSSettings(..))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types                        (Method,methodGet,methodPost)
import           Network.Transport.ZMQ                     (createTransport, defaultZMQParameters)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO                                 (hClose, hGetContents, hPutStrLn, stderr)
import           System.Process                            (readProcess)
--
import           Type
import           Util.Json


foreign import ccall "json_create"    c_json_create   :: CString -> IO Json_t
foreign import ccall "&json_finalize" c_json_finalize :: FunPtr (Json_t -> IO ())
foreign import ccall "json_serialize" c_json_serialize :: Json_t -> IO CString



foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query"          c_query          :: Json_t -> IO Json_t
foreign import ccall "query_finalize" c_query_finalize :: IO ()

type Json = ForeignPtr RawJson

json_create :: CString -> IO Json
json_create cstr = c_json_create cstr >>= newForeignPtr c_json_finalize

query :: Json -> IO Json
query q = withForeignPtr q c_query >>= newForeignPtr c_json_finalize

json_serialize :: Json -> IO CString
json_serialize p = withForeignPtr p c_json_serialize

runCoreNLP body = do
  -- let body = "establishing \rthe ecological criteria for awarding the EU ecolabel"
  -- body <- TE.encodeUtf8 <$> TIO.getContents 
  lbstr <- simpleHttpClient methodPost "http://192.168.1.104:9000/?properties={%22annotators%22%3A%22depparse%2Cpos%22%2C%22outputFormat%22%3A%22json%22}" (Just body)
  -- BL.toStrict lbstr
  let txt' = TE.decodeUtf8 (BL.toStrict lbstr)
      txt = T.filter (>=' ') txt'
      r_bstr = TE.encodeUtf8 txt
  let Right (Object c) = A.parseOnly json r_bstr
      c' = Object (HM.insert "queries" (Array (V.fromList [String (TE.decodeUtf8 body)])) c)
  print c'
  -- TIO.putStrLn txt
  -- print txt

  
  (return . BL.toStrict . B.toLazyByteString . encodeToBuilder) c' -- r_bstr
  
queryWorker :: SendPort BL.ByteString -> Query -> Process ()
queryWorker sc q = do
  -- let r = encode (makeJson q)
  --     bstr = BL.toStrict r
  bstr <- (liftIO . runCoreNLP . B.pack . head . querySentences) q
  bstr' <- liftIO $ B.useAsCString bstr $ 
    json_create >=> query >=> json_serialize >=> unsafePackCString
  liftIO $ B.putStrLn bstr'
  sendChan sc (BL.fromStrict bstr')
  return ()
  
simpleHttpClient :: Method -> String -> Maybe ByteString -> IO BL.ByteString
simpleHttpClient mth url mbstr = do
  request0 <- parseRequest url
  let request = maybe (request0 { method = mth }) (\bstr -> request0 { method = mth, requestBody = RequestBodyBS bstr }) mbstr
  manager <- if (secure request)
               then do
                 let tlssetting = TLSSettingsSimple True False False
                     mansetting = mkManagerSettings tlssetting Nothing
                 newManager mansetting
               else newManager defaultManagerSettings
  response <- httpLbs request manager
  return (responseBody response)
  
server :: String -> Process ()
server url = do
  str <- liftIO (BL.unpack <$> simpleHttpClient methodGet url Nothing)
  runMaybeT $ do
    m <- (MaybeT . return) (Data.Aeson.decode (BL.pack str)) :: MaybeT Process (M.Map String String)
    pidstr <- (MaybeT . return) (M.lookup "result" m)
    liftIO $ hPutStrLn stderr (show pidstr)
    let them = (Bi.decode . B64.decodeLenient . BL.pack) pidstr
    lift $ do
      let heartbeat n = send them (HB n) >> liftIO (threadDelay 5000000) >> heartbeat (n+1)
      us <- getSelfPid
      (sc,rc) <- newChan :: Process (SendPort Query, ReceivePort Query)
      send them (sc,us)
      sc' <- expect :: Process (SendPort BL.ByteString)
      spawnLocal (heartbeat 0)
      forever $ do
        q <- receiveChan rc
        liftIO $ hPutStrLn stderr (show q)
        spawnLocal (queryWorker sc' q)
  return ()

makeJson :: Query -> Value
makeJson (Query qs) = object [ "queries" .= toJSON qs ]

main = do
  configurl <- liftIO (getEnv "CONFIGURL")
  
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  
  withCString "config.json" $ \configfile -> do
    c_query_init configfile
    runProcess node (server configurl)
    c_query_finalize

