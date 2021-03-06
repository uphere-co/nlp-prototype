import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Resource (ResourceT,runResourceT)
import           Control.Monad.Trans.State (StateT(runStateT),
                                            put,get,
                                            modify')

import           Data.Default (Default,def)
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Conduit (Sink,Source,
                               ($$),await,awaitForever)
import           Data.XML.Types (Event (EventBeginElement,
                                        EventEndElement,
                                        EventContent),
                                 Content(ContentText),
                                 Name(nameLocalName))
import           Text.XML.Stream.Parse (parseFile)

fileName = "./ipg160105.xml" :: FilePath 
                            
instance Default DocState where
    def = DocState 0 "" "" 0

-- (parseFile def fileName) ::
--      Source (ResourceT IO) Event
-- (runStateT myprocess def) ::
--      Sink Event (ResourceT IO) ((),DocState)
-- (parseFile def fileName) $$ (runStateT myprocess def) ::
--      (ResourceT IO) ((),DocState)
--main ::  IO ((),DocState)
--main = runResourceT $
--       parseFile def fileName $$ runStateT myprocess def

-- Let's use awaitForever

data Doc 
    = Doc { docSerial :: Int
          , docNumber :: Text
       -- , docDescText :: Text
          , docDescLength :: Int }
    deriving Show

type DocState = StateT IO Doc

sourceXMLevent :: FilePath -> Source DocState Event
sourceXMLevent fn = do

updateDocState :: Conduit Event DocState Event
updateDocState = do

showContent ::
    Name -> Conduit Event DocState Event
showContent name =
    awaitForever catch
  where
    catch ev =
      case ev of
        EventBeginElement name attrs
          -> showIt
        _ -> return () 
    showIt =
      mev <- await
      case mev of
        Just EventContent (ContentText content)
          -> liftIO $ putStrLn . T.unpack content
        _ -> return ()

catchDescription = catchContent "description" hook
  where hook = \content -> print . T.unpack content

catchFig = catchContent "figure" hook
  where hook = \content -> 

    

descriptionCatcher = elementCatcher descHook
  where descHook = \ev -> st

sink :: Sink Event DocState (IO ())
sink = do

main :: IO ()
main = do
        sourceXMLevent 
     $$ updateDocState
    =$= catcherDescription
    =$= catcherFigure
     $$ sinkAll

-- state :: DocState
-- n :: Int
-- r :: Bool
myprocess ::
    StateT DocState (Sink Event IO) ()
myprocess = do
    docState <- get
    let n = doc_serial docState
    r <- findBegin "us-patent-grant" singlePatent
    if r then do
        docState' <- get
        liftIO . putStrLn
               $  "==============" ++ show n ++ ":"
               ++ T.unpack (doc_number docState')
               ++ show ( doc_number docState' )
               ++ "======================================"
        put def { doc_serial = n+1 }
        myprocess
    else return ()
  where
    singlePatent = docNumber >> description  
  
-- await :: Consumer Event IO (Maybe Event)
--         == forall o.
--              ConduitM Event o IO (Maybe Event)
-- lift :: m a -> StateT DocState m a
-- lift await :: forall o.
--                  StateT DocState
--                      (ConduitM Event o IO)
--                      (Maybe Event)
findBegin ::
    Text
    -> StateT DocState (Sink Event IO) Bool
    -> StateT DocState (Sink Event IO) Bool 
findBegin tag begin = do
    mx <- lift await
    case mx of
      Nothing -> return False
      Just x  -> do
          case x of
            EventBeginElement name attrs ->
                if nameLocalName name == tag then
                    begin >> return True
                else findBegin tag begin
            _ -> findBegin tag begin

findEnd ::
    Text
    -> ( Event -> StateT DocState (Sink Event IO) () )
    -> StateT DocState (Sink Event IO) Bool
findEnd tag inner = do
    mx <- lift await
    case mx of
      Nothing -> return False
      Just ev -> do
          case ev of
            EventEndElement name -> do
                if nameLocalName name == tag then
                    return True
                else findEnd tag inner
            _ -> inner ev >> findEnd tag inner

docNumber ::
    StateT DocState (Sink Event IO) Bool
docNumber =
    findBegin "doc-number" $
    findEnd "doc-number" getDocNumber
  where
    getDocNumber ev =
        case ev of
          EventContent (ContentText text) ->
                modify' (\st -> st { doc_number = text })
          _ -> return ()

description ::
    StateT DocState (Sink Event IO) Bool
description =
    findBegin "description" $
    findEnd "description" getDescription
  where
    getDescription ev = do
        case ev of
          EventBeginElement name attrs -> do
              if name `elem` ["tables", "maths","chemistry"] then
                  skipElement name
              else return ()
              mx <- lift await
              case mx of
                Nothing -> return ()
                Just ev -> getDescription ev
          EventContent (ContentText text) -> do
              let len = T.length text
              modify' $ \st -> let n = doc_desc_length st
                                   tevt = doc_desc_text st
                               in st { doc_desc_length = n + len }
              liftIO . putStr $ T.unpack text
          _ -> return ()   

skipElement ::
    Name -> StateT DocState (Sink Event IO) ()
skipElement tag = do
    mx <- lift await
    case mx of
      Nothing -> return ()
      Just x -> do
          case x of
            EventEndElement tag -> return ()
            _ -> skipElement tag

