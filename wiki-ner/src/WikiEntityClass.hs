module WikiEntityClass where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Map                      as M
import           Data.Map                              (Map)
import           Data.Maybe                            (fromMaybe,fromJust)

import           NamedEntity                           (NamedEntityClass)
import qualified WikiEntity                    as Wiki



type NEClass = NamedEntityClass

loadTypedUIDs :: (NEClass , FilePath) -> IO [(Wiki.UID, NEClass)]
loadTypedUIDs (tag, fileName) = do
  content <- T.IO.readFile fileName
  let 
    uids = map (\x -> (Wiki.UID x, tag)) (T.lines content)
  return uids

data WikiUID2NETag = WikiUID2NETag { _map :: Map Wiki.UID NEClass}
                   deriving (Show)

loadWikiUID2NETag :: [(NEClass, FilePath)] -> IO WikiUID2NETag
loadWikiUID2NETag pairs = do
  lists <- mapM loadTypedUIDs pairs
  let
    table = WikiUID2NETag (M.fromList (mconcat lists))
  return table

getNEClass :: WikiUID2NETag -> Wiki.UID -> NEClass
getNEClass table uid = f (M.lookup uid (_map table))
  where 
    f (Just x) = x
    f _ = error ("Unknown UID: " ++ T.unpack (Wiki._uid uid))
