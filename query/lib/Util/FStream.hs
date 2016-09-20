{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Util.FStream where

import           Control.Monad.IO.Class           (MonadIO (..))
import           Foreign.C.Types
import           Foreign.Ptr
import           System.Posix.Types               (Fd(..))
--
import           Util.Pipe

data RawIStream
data RawOStream

-- newtype IStream = IStream (Ptr RawIStream)
-- newtype OStream = OStream (Ptr RawOStream)

type IStream = Ptr RawIStream
type OStream = Ptr RawOStream

foreign import ccall "make_istream_from_fd" c_make_istream_from_fd :: Fd -> IO (Ptr RawIStream)
foreign import ccall "make_ostream_from_fd" c_make_ostream_from_fd :: Fd -> IO (Ptr RawOStream)
foreign import ccall "delete_istream" c_delete_istream :: IStream -> IO ()
foreign import ccall "delete_ostream" c_delete_ostream :: OStream -> IO ()



mkStreamPairFromDuplex :: PipeDuplex -> IO (IStream,OStream)
mkStreamPairFromDuplex PipeDuplex{..} = do
  let fi = pp_out hereToThere   -- note that in/out is reversed!
      fo = pp_in thereToHere
  is <- c_make_istream_from_fd fi
  os <- c_make_ostream_from_fd fo
  return (is,os)

withStreamPairFromDuplex :: (MonadIO m) =>
                            PipeDuplex
                         -> ((IStream,OStream) -> m a)
                         -> m a
withStreamPairFromDuplex duplex f = do
  (is,os) <- liftIO $ mkStreamPairFromDuplex duplex
  r <- f (is,os)
  liftIO $ c_delete_istream is
  liftIO $ c_delete_ostream os
  return r
