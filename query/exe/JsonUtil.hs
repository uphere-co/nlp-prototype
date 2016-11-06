{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonUtil where

import Data.Aeson
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
--
import           QueryServer.Type

data RawJson
type Json_t = Ptr RawJson

type Json = ForeignPtr RawJson


foreign import ccall "json_create"    c_json_create   :: CString -> IO Json_t
foreign import ccall "&json_finalize" c_json_finalize :: FunPtr (Json_t -> IO ())
foreign import ccall "json_serialize" c_json_serialize :: Json_t -> IO CString


makeJson :: Query -> Value
makeJson (Query qs) = object [ "queries" .= toJSON qs ]

json_serialize :: Json -> IO CString
json_serialize p = withForeignPtr p c_json_serialize

json_create :: CString -> IO Json
json_create cstr = c_json_create cstr >>= newForeignPtr c_json_finalize
