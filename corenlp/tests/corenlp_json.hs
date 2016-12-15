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
newtype DepPos = DepPos Int
             deriving (Ord, Eq, Show, ToJSON, FromJSON)
newtype Gov = Gov Text
             deriving (Eq, Show, ToJSON, FromJSON)
newtype GovPos = GovPos Int
             deriving (Ord, Eq, Show, ToJSON, FromJSON)

data DepToken = DepToken  { arclabel :: ArcLabel
                            , dep      :: Dep
                            , dep_pos  :: DepPos
                            , gov      :: Gov
                            , gov_pos  :: GovPos }
                           deriving (Eq, Show)

unDepPos (DepPos x) = x
unGovPos (GovPos x) = x

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

tryDepChunk :: Either String DepChunk -> DepChunk
tryDepChunk (Right chunk) = chunk

assert :: Bool -> String
assert True = "Test passed"
assert False = error "error occurred" 


--- 20161213

type Vertex = Int

type Edge = (Vertex,Vertex)
 
--                 parent  child
type Graph = M.Map Vertex [Vertex]

-- Leaf (Just n)
-- Leaf Nothing 

data Tree = Leaf Vertex
          | Node Vertex [Tree]
          deriving Show

data TreeT = LeafT (Maybe Vertex)
           | NodeT Vertex [TreeT]
           deriving Show


{- 
data TreeWithP = LeafP (Maybe Vertex,Vertex)
               | NodeP (Maybe Vertex,Vertex) [TreeWithP]

data Tree a = Leaf a
            | Node a [Tree a]

type SimpleTree = Tree Vertex
type TreeWithP = Tree (Maybe Vertex,Vertex)

buildWithP :: SimpleTree -> TreeWithP
-}            


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
buildWordTable ws = M.fromList (map (\w -> (unDepPos (word_pos w),(word w))) ws) 

buildTree :: Graph -> Tree
buildTree g = go 0   
  where go n = case mds of
                 Nothing -> Leaf n
                 Just ds -> Node n (map go ds)
          where mds = M.lookup n g

elim :: Vertex -> Tree -> TreeT
elim n (Leaf m)    | n == m    = LeafT Nothing
                   | otherwise = LeafT (Just m)
elim n (Node m ds) | n == m    = LeafT Nothing
                   | otherwise = NodeT m (map (elim n) ds)
                


-- prune :: TreeT -> Tree

-- put parent info to each node using general tree type.

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


---------------------------
-- Jihun's trial:
isLeaf pos gov2deps = let r = M.lookup (GovPos pos) gov2deps
                      in case r of
                          Nothing -> True
                          Just _  -> False
                          
buildDependentsMap es = foldl' update M.empty es
  where 
    update acc (g,d) = let f Nothing = Just [d]
                           f (Just ds) = Just (d:ds)
                       in M.alter f g acc

governorIndex idx dep2gov = let r = M.lookup (DepPos idx) dep2gov
                            in case r of
                               Nothing         -> Nothing
                               Just (GovPos i) -> Just i

toRoot idx heads dep2gov = let r = governorIndex idx dep2gov
                           in case r of
                               Nothing -> (idx:heads)
                               Just i -> toRoot i (idx:heads) dep2gov

{-
jsonstr <- BL.readFile "data/sent.json"
ej = eitherDecode jsonstr :: Either String DepChunk
chunk = tryDepChunk ej
ds =  deps $ head $ sents chunk

sents_deps = fmap deps $ sents chunk
sent_deps = head sents_deps
all_nodess = fmap (fmap $ unDepPos.dep_pos ) sents_deps
all_nodes = head all_nodess

dep2gov = M.fromList $ fmap (\x -> (dep_pos x, gov_pos x)) sent_deps
gov2deps = buildDependentsMap $ fmap (\x -> (gov_pos x, dep_pos x)) sent_deps

all_leaf = filter (\x -> isLeaf x gov2deps) all_nodes
all_paths = map (\x -> toRoot x [] dep2gov) all_leaf

-- list => map, "fold", concat, concatMap, take, break, scanl` 
-}

-- type FilePath = String 
-- Prelude.readFile :: FilePath -> IO String
-- BL.readFile :: FilePath -> IO BL.ByteString
-- T.IO.readFile :: FilePath -> IO Text

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
      print css
      print (fmap (fmap (replaceIndex2Word wmap)) css)

      wuidstr <- readFile "../../rnn++/tests/data/words.uid"
      wimpstr <- readFile "../../rnn++/tests/data/word_importance"

      let wuids = lines wuidstr
          wimps0 = lines wimpstr 
      print $ zip wuids wimps0

      -- lines :: String -> [String]
      -- zip :: [a] -> [b] -> [(a,b)]

      print $ zip3 [1..] wuids wimps0 

      print $ zipWith (++) wuids wimps0    -- zipWith :: (a -> b-> c) -> [a] -> [b] -> [c]
                                                              -- map :: (a -> b) -> [a] -> [b]
      -- zipWith f xs ys = map (uncurry f) (zip xs ys)
      -- uncurry f (x,y) = f x y
      -- flip f x y = f y x

      -- class Show a  where show :: a -> String
      -- class Read a  where read :: String -> a
      print (read "1.2" :: Double)

      let wimps :: [Double]
          wimps = map read wimps0
      print (zip wuids wimps)

      -- [(gov,ds)]
      -- [edge]

      -- (gov, [(gov,(gov, null) ) ... ]
      let tr = buildTree g
      print tr
      print (elim 15 tr)
      -- print (prune (elim 15 tr))
