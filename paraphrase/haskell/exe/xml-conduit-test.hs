import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.Resource (ResourceT,runResourceT)

import Data.Default (Default,def)
import Data.Text (Text)
import qualified Data.Text as T

import Data.ByteString (ByteString,uncons,singleton)
import qualified Data.ByteString.Char8 as C8

import Data.Conduit (Sink,Source,Conduit,($$),($=),(=$),(=$=),yield,await,awaitForever)
import Data.Conduit.Binary (sourceFile,isolate)
import Data.XML.Types (Event (EventBeginElement,EventEndElement,EventContent),Content(ContentText),Name(nameLocalName))
import Text.XML.Stream.Parse (parseFile,parseBytes)

fileName = "./test.xml" :: FilePath

main :: IO ()
main =
    runResourceT $
      sourceFile fileName
       $$  chopper
       =$= showByteString
       =$= parseBytes def
       =$  showEvent

-- Is there a better way ?
chopper :: MonadIO m => Conduit ByteString m ByteString
chopper = awaitForever $ \b -> go b
  where
    go b =
      case uncons b of
        Just (w,b') -> do
          yield (singleton w) >> go b'
        Nothing -> return ()

showByteString :: MonadIO m => Conduit ByteString m ByteString
showByteString = awaitForever $ \b -> do 
    liftIO $ putStr $ C8.unpack b
    yield b

showEvent :: Sink Event (ResourceT IO) ()
showEvent = awaitForever $ \ev ->
    liftIO $ printRed ev
  where
    printRed s = putStrLn $ "\x1b[31m" ++ show s ++ "\x1b[39m"
