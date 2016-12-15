{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative               ((<|>))
import           Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as M   (Map, alter, empty, fromList, lookup, map)
import           Data.Maybe                        (fromJust, maybeToList)
import           Data.List                         (foldl')
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T.E
import qualified Data.Text.IO               as T.IO
import           GHC.Generics

import           Prelude      hiding (words)


newtype ArcLabel = ArcLabel Text
             deriving (Eq, Show, ToJSON, FromJSON)
newtype Dep = Dep Text
             deriving (Eq, Show, ToJSON, FromJSON)
newtype DepPos = DepPos { unDepPos :: Int }
             deriving (Eq, Show, ToJSON, FromJSON)
newtype Gov = Gov Text
             deriving (Eq, Show, ToJSON, FromJSON)
newtype GovPos = GovPos Int
             deriving (Eq, Show, ToJSON, FromJSON)

data DepToken = DepToken  { arclabel :: ArcLabel
                            , dep      :: Dep
                            , dep_pos  :: DepPos
                            , gov      :: Gov
			    , gov_pos  :: GovPos }
            deriving (Eq, Show)

{-
data DepToken2 = DepToken2  { dep2 :: ArcLabel
                            , dependentGloss      :: Dep
                            , dependent  :: DepPos
                            , governorGloss      :: Gov
			    , governor  :: GovPos }
--            deriving (Eq, Show, FromJSON, ToJSON)
            deriving (Eq, Show, Generic)
instance FromJSON DepToken2
instance ToJSON DepToken2
-}

newtype POS = POS Text
            deriving (Eq, Show, ToJSON, FromJSON)
newtype Offset = Offset Int
               deriving (Eq, Show, ToJSON, FromJSON)
newtype WordOriginal = WordOriginal Text
                     deriving (Eq, Show, ToJSON, FromJSON)

data WordToken = WordToken  { word_pos :: DepPos
                              , word     :: Dep
                              , pos      :: POS
                              , offset_front :: Offset
                              , offset_back  :: Offset
                              , word_orig    :: WordOriginal }
               deriving (Eq, Show)

data SentenceDep = SentenceDep { deps  :: [DepToken]
                               , words :: [WordToken] }
            deriving (Eq, Show)
data DepChunk = DepChunk { sents :: [SentenceDep] }
            deriving (Eq, Show)

instance ToJSON DepToken where
  toJSON (DepToken {..}) -- arc dep dep_pos gov gov_pos
    = object [ "dep" .= arclabel,  "dependent" .= dep_pos,  "dependentGloss" .= dep,  "governor" .= gov_pos, "governorGloss" .= gov ] 

instance FromJSON DepToken where
  parseJSON (Object v) =
    (DepToken <$> v .: "dep" <*> v .: "dependentGloss"<*> v .: "dependent"<*> v .: "governorGloss"<*> v .: "governor")
  parseJSON wat = Data.Aeson.Types.typeMismatch "DepToken" wat 


instance ToJSON WordToken where
  toJSON (WordToken word_pos word pos offset_front offset_back word_orig)
    = object [ "index" .= word_pos,  "word" .= word,  "pos" .= pos,  
               "characterOffsetEnd" .= offset_front, "characterOffsetBegin" .= offset_back,
               "originalText" .= word_orig ] 

instance FromJSON WordToken where
  parseJSON (Object v) =
    (WordToken <$> v .: "index" <*> v .: "word"<*> v .: "pos" 
                 <*> v .: "characterOffsetEnd"<*> v .: "characterOffsetBegin"
                 <*> v .: "originalText")
  parseJSON wat = Data.Aeson.Types.typeMismatch "WordToken" wat 



instance ToJSON SentenceDep where
  toJSON (SentenceDep deps words)
    = object [ "basicDependencies" .= deps,  "tokens" .= words ] 

instance FromJSON SentenceDep where
  parseJSON (Object v) =
    (SentenceDep <$> v .: "basicDependencies" <*> v .: "tokens" )
  parseJSON wat = Data.Aeson.Types.typeMismatch "SentenceDep" wat 

instance ToJSON DepChunk where
  toJSON (DepChunk sents)
    = object [ "sentences" .= sents ] 

instance FromJSON DepChunk where
  parseJSON (Object v) =
    (DepChunk <$> v .: "sentences" )
  parseJSON wat = Data.Aeson.Types.typeMismatch "DepChunk" wat 


tryDump :: Either String DepChunk -> BL.ByteString
tryDump (Right json) = encode json
tryDump (Left _ )    = "null"


assert :: Bool -> String
assert True = "Test passed"
assert False = error "error occurred" 


--- 20161213

type Vertex = Int

type Edge = (Vertex,Vertex)
 
--                 parent  child
type Graph = M.Map Vertex [Vertex]

mkEdge :: DepToken -> Edge
mkEdge (DepToken _ _ (DepPos d1) _ (GovPos g1)) = (g1,d1)

mkEdges :: [DepToken] -> [Edge]
mkEdges ds = map mkEdge ds

buildGraph :: [Edge] -> Graph
buildGraph es = foldl' update M.empty es
  where
    -- Nothing (no value with key)    ->   Just v' , Nothing 
    -- Just v                         ->  Just v', Nothing 
    update acc (g,d) = let f Nothing = Just [d]
                           f (Just ds) = Just (d:ds)
                       in M.alter f g acc
                 
buildWordTable :: [WordToken] -> M.Map Vertex Dep
buildWordTable ws = M.fromList (map (\w -> ( unDepPos (word_pos w),(word w) )) ws) 



-- data Maybe a = Nothing    (analogous to null, std::optional)
--              | Just a 

replaceIndex2Word :: M.Map Vertex Dep -> Vertex -> Dep
replaceIndex2Word wordmap 0 = Dep "ROOT"
replaceIndex2Word wordmap v = fromJust (M.lookup v wordmap)


-- (k is an instance of Ord typeclass)
-- replaceIndex2Word :: (Ord k) => M.Map k v -> k -> v
-- replaceIndex2Word m v = fromJust (M.lookup v m)


-- replaceIndex2WordInGraph :: M.Map Vertex Dep -> M.Map Vertex [Vertex] -> M.Map Vertex [Dep]
-- replaceIndex2WordInGraph wordmap graph = M.map (\vs -> map (\v -> replaceIndex2Word wordmap v) vs) graph 

-- by currying
replaceIndex2WordInGraph :: M.Map Vertex Dep -> M.Map Vertex [Vertex] -> M.Map Vertex [Dep]
replaceIndex2WordInGraph wordmap graph = M.map (map (replaceIndex2Word wordmap)) graph 


-- replaceIndex2WordInGraph :: M.Map Vertex Dep -> M.Map Vertex [Vertex] -> M.Map Vertex [Dep]
-- replaceIndex2WordInGraph wordmap graph = fmap (fmap (replaceIndex2Word wordmap)) graph 

allDepChain :: Graph -> [[Vertex]]
allDepChain g = depChain g 0

depChain :: Graph -> Vertex -> [[Vertex]]
depChain g v = let mds = M.lookup v g
               in case mds of
	            Nothing -> [[v]]
	            Just ds -> let css = concat (map (\x-> depChain g x) ds)
		               in map (\cs -> v:cs) css

-- -> list monad: concatMap = concat . map  is (>>=) of list monad.  

-- maybeToList Nothing = []
-- maybeToList (Just x) = [x]


-- dc   [[Vertex]]
-- [[[Vertex]]]

-- ds = [d1, d2, ... ]
-- depChain g d1 = [c1,c2,c3]
-- [ v:c1, v:c2, v:c3 ]


-- null :: [a] -> Bool 


{- 
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map (:: (a -> b) -> [a] -> [b] )

instance Functor (M.Map k) where
  fmap = M.map (:: (a -> b) -> M.Map k a -> M.Map k b 


<$> = fmap

fmap f xs = f <$> xs
DepParse <$> o.= "name" 

-}

-- [a] ( fmap = map)
-- [a] = [] a   -- template vector<T>    -- type constructor  ; [], Map k 
-- Map k v ( fmap = M.map)  -- template map<K,V>  ---> currying (map k) v   


-- (f . g) (x) = f (g (x))


-- f x y ~ f (x,y)

-- f x = \y -> f x y  -- x is captured variable   [x](return f(x,y);)

--[a] -> [b] 
--map :: (a -> b) -> [a] -> [b]
--map :: (a -> b) -> Map k [Vertex] -> Map k [Dep] 



main :: IO ()
main = do
  jsonstr <- BL.readFile "data/dep_token.json"
  print (eitherDecode jsonstr :: Either String DepToken)
  
  jsonstr <- BL.readFile "data/word_token.json"
  print (eitherDecode jsonstr :: Either String WordToken)

  jsonstr <- BL.readFile "data/sent.json"
  let
    ej = eitherDecode jsonstr :: Either String DepChunk
    jsondump = tryDump ej
    ejd = eitherDecode jsondump :: Either String DepChunk
  print.assert$ ej==ejd

  case ejd of
    Left err -> error err
    Right jd -> do
      let s = head (sents jd)
          ds = deps s
	  ws = words s
          g = buildGraph (mkEdges ds)
      print g
      let wmap = buildWordTable ws
      print wmap
      print (replaceIndex2Word wmap 3)
      print (replaceIndex2WordInGraph wmap g)
      let css = allDepChain g 
      print (fmap (fmap (replaceIndex2Word wmap)) css)