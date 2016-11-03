{-# LANGUAGE EmptyDataDecls #-}
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
import           Data.Aeson.Types
import qualified Data.Attoparsec                     as A
import qualified Data.Binary                         as Bi (encode,decode)
import qualified Data.Binary.Builder                 as B  (toLazyByteString)
import qualified Data.ByteString.Base64.Lazy         as B64
import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Char8               as B

import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.ByteString.Unsafe                    (unsafeUseAsCStringLen,unsafePackCString)
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Map                            as M
import           Data.Maybe                                (maybeToList)
import           Data.Scientific

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

data RawJson
type Json_t = Ptr RawJson

foreign import ccall "json_create"    c_json_create   :: CString -> IO Json_t
foreign import ccall "&json_finalize" c_json_finalize :: FunPtr (Json_t -> IO ())
foreign import ccall "json_serialize" c_json_serialize :: Json_t -> IO CString



foreign import ccall "query_init"     c_query_init     :: CString -> IO ()
foreign import ccall "query"          c_query          :: Json_t -> IO Json_t
foreign import ccall "query_finalize" c_query_finalize :: IO ()

type Json = ForeignPtr RawJson

data NLPResult = NLPResult [Sentence] deriving Show
instance FromJSON NLPResult where
  parseJSON (Object o) = NLPResult <$> o .: "sentences"
  parseJSON invalid = typeMismatch "NLPResult" invalid

data Sentence = Sentence { unSentence :: [Token]} deriving Show

instance FromJSON Sentence where
  parseJSON (Object o) = Sentence <$> o .: "tokens"
  parseJSON invalid = typeMismatch "Sentence" invalid


data Token = Token { unToken :: Text} deriving Show

instance FromJSON Token where
  parseJSON (Object o) = Token <$> o .: "word"
  parseJSON invalid = typeMismatch "Token" invalid

json_create :: CString -> IO Json
json_create cstr = c_json_create cstr >>= newForeignPtr c_json_finalize

query :: Json -> IO Json
query q = withForeignPtr q c_query >>= newForeignPtr c_json_finalize

json_serialize :: Json -> IO CString
json_serialize p = withForeignPtr p c_json_serialize

runCoreNLP body = do
  lbstr <- simpleHttpClient False methodPost "http://192.168.1.104:9000/?properties={%22annotators%22%3A%22depparse%2Cpos%22%2C%22outputFormat%22%3A%22json%22}" (Just body)
  let r_bstr = BL.toStrict lbstr
  let Just c' = do
        o@(Object c) <- A.maybeResult (A.parse json r_bstr)
        NLPResult ss <- decodeStrict' r_bstr
        let queries = map (String . T.intercalate " " . map unToken . unSentence) ss 
        return . Object . HM.insert "max_clip_len" (toJSON (200 :: Int)) . HM.insert "queries" (Array (V.fromList queries)) $ c
  -- print c'
  (return . BL.toStrict . B.toLazyByteString . encodeToBuilder) c' -- r_bstr
  
queryWorker :: SendPort BL.ByteString -> Query -> Process ()
queryWorker sc q = do
  liftIO $ print q
  let failed =encode Null
      ss = querySentences q
  case ss of
    (s:_) -> do
      if (not . T.null) s
        then do
          bstr <- (liftIO . runCoreNLP . TE.encodeUtf8) s
          bstr' <- liftIO $ B.useAsCString bstr $ 
            json_create >=> query >=> json_serialize >=> unsafePackCString
          liftIO $ B.putStrLn bstr'
          sendChan sc (BL.fromStrict bstr')
        else 
          sendChan sc failed 
    [] -> sendChan sc failed
  
simpleHttpClient :: Bool -> Method -> String -> Maybe ByteString -> IO BL.ByteString
simpleHttpClient isurlenc mth url mbstr = do
  request0' <- parseRequest url
  let request0 = request0' { requestHeaders = requestHeaders request0' ++ [ ("Accept","application/json") ] }
  let request' = maybe (request0 { method = mth }) (\bstr -> request0 { method = mth, requestBody = RequestBodyBS bstr }) mbstr
      request = if isurlenc then urlEncodedBody [] request' else request'
  print (requestHeaders request)
  case mbstr of
    Nothing -> return ()
    Just bstr -> TIO.putStrLn (TE.decodeUtf8 bstr)
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
  str <- liftIO (BL.unpack <$> simpleHttpClient False methodGet url Nothing)
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
makeJson (Query qs) = object [ "queries" .= toJSON qs
                             -- , "max_clip_len" .= (200 :: Int)
                             ]

main = do
  configurl <- liftIO (getEnv "CONFIGURL")
  
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (B.pack host)
  node <- newLocalNode transport initRemoteTable
  
  withCString "config.json" $ \configfile -> do
    c_query_init configfile
    runProcess node (server configurl)
    c_query_finalize

