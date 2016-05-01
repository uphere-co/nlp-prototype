{-# LANGUAGE OverloadedStrings #-}

module NLP.WordVector.Parser where

import           Control.Monad.IO.Class (MonadIO(..), liftIO)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Char                  (isSpace)
import           Data.Conduit               ((=$=))
import qualified Data.Conduit        as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import           Data.Text                  (Text)
import qualified Data.Text.Encoding  as TE
import           Data.Vector.Storable       (Vector)
import qualified Data.Vector.Storable as V
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Unsafe.Coerce

skipSpace :: (MonadResource m) => C.Sink B.ByteString m ()
skipSpace = CB.dropWhile (isSpace . unsafeCoerce)

getVector :: (MonadResource m, MonadIO m) => Int -> C.Sink B.ByteString m (Text,Vector Float)
getVector n = do
    w <- head <$> (CB.takeWhile (not . isSpace . unsafeCoerce) =$= CL.consume)
    skipSpace
    vbstr <- LB.toStrict <$> CB.take (4*n)
    v <- liftIO . B.useAsCString vbstr $ \cstr -> do
      nstr <- mallocBytes (4*n) 
      copyBytes nstr cstr (4*n)
      fptr <- castForeignPtr <$> newForeignPtr_ nstr
      return (V.unsafeFromForeignPtr0 fptr n)
    skipSpace
    case TE.decodeUtf8' w of
      Right w' -> return (w',v)
      Left err -> return ("_error",v)
    -- return (TE.decodeUtf8 w,v)

normalize :: Vector Float -> Vector Float
normalize v =  
    let l2 = V.sum (V.map (\x->x*x) v)
    in V.map (\x->x/sqrt l2) v

