module Network.Util where

import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar,newTMVarIO,putTMVar,takeTMVar)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.Binary                 as Bi
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import qualified Network.Simple.TCP          as NS
import           System.IO                         (hFlush, hPutStrLn, stderr)

recvAndUnpack :: Bi.Binary a => NS.Socket -> IO (Maybe a)
recvAndUnpack sock = do
  msizebstr <- NS.recv sock 4
  case msizebstr of
    Nothing -> return Nothing
    Just sizebstr -> do
      let s32 = (Bi.decode . BL.fromStrict) sizebstr :: Bi.Word32
          s = fromIntegral s32 :: Int
      mmsg <- NS.recv sock s
      case mmsg of
        Nothing -> return Nothing
        Just msg -> (return . Just . Bi.decode . BL.fromStrict) msg

packNumBytes :: B.ByteString -> B.ByteString
packNumBytes bstr =
  let len = (fromIntegral . B.length) bstr :: Bi.Word32
  in BL.toStrict (Bi.encode len)

packAndSend :: (Bi.Binary a) => NS.Socket -> a -> IO ()
packAndSend sock x = do
  let msg = (BL.toStrict . Bi.encode) x
      sizebstr = packNumBytes msg
  NS.send sock sizebstr
  NS.send sock msg



type LogLock = (TMVar (),Int)

newLogLock n = liftIO $ (,) <$> newTMVarIO () <*> pure n

atomicLog lock str = liftIO $ do
  let n = snd lock
  atomically $ takeTMVar (fst lock)
  let result = BC.pack ("[" ++ show n ++ "]: " ++ str)
  result `seq` B.hPutStrLn stderr result
  hFlush stderr
  atomically $ putTMVar (fst lock) ()

getClientNum (l,n) = n
incClientNum (l,n) = (l,n+1)
