{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Util.Json where

import Foreign.Ptr

data RawJson

type Json_t = Ptr RawJson



