{-# LANGUAGE RecordWildCards #-}

module Util.Pipe where

import qualified Data.ByteString.Lazy.Char8 as BL

import           Data.Conduit                     (runConduit, ($$))
import           Data.Conduit.Binary              (sourceLbs,sourceHandle,sinkLbs,sinkHandle) 
import           System.Posix.IO                  (closeFd, createPipe, fdToHandle, fdWrite)
import           System.Posix.Types               (Fd(..))

data PipePair = PP { pp_out :: Fd, pp_in :: Fd }

pairToPP :: (Fd,Fd) -> PipePair
pairToPP (fo,fi) = PP fo fi 

data PipeDuplex = PipeDuplex { hereToThere :: PipePair
                             , thereToHere :: PipePair
                             }

mkDuplex :: IO PipeDuplex
mkDuplex = PipeDuplex <$> (pairToPP <$> createPipe) <*> (pairToPP <$> createPipe)

pipeTransmit :: PipeDuplex -> BL.ByteString -> IO BL.ByteString
pipeTransmit PipeDuplex{..} bstr = do
  hq <- fdToHandle (pp_in hereToThere) 
  hr <- fdToHandle (pp_out thereToHere)
  runConduit $ sourceLbs bstr $$ sinkHandle hq
  runConduit $ sourceHandle hr $$ sinkLbs


