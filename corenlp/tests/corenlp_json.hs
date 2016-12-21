{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative               ((<|>))
import           Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as M   (Map, delete, alter, empty, fromList, toList, lookup, map)
import           Data.Maybe                        (fromJust, maybeToList,catMaybes,listToMaybe)
import           Data.List                         (foldl')
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T.E
import qualified Data.Text.IO               as T.IO
import           GHC.Generics

import           Prelude      hiding (words)

newtype WUID = WUID Int
             deriving (Ord, Eq, Show, ToJSON, FromJSON)

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

-- Question : what is f??                          
buildDependentsMap edges = foldl' update M.empty edges
  where update acc (g,d) = let f Nothing   = Just [d]
                               f (Just ds) = Just (d:ds)
                           in M.alter f g acc

toRoot idx dep2gov = toRoot_o idx [] dep2gov
  where toRoot_o idx heads dep2gov = let r = M.lookup (DepPos idx) dep2gov
            in case r of
              Nothing -> (idx:heads)
              Just (GovPos i) -> toRoot_o i (idx:heads) dep2gov

toGov = (\(DepPos x) -> (GovPos x))
toDep = (\(GovPos x) -> (DepPos x))

data DepTree a = Empty
               | DLeaf a
               | DNode a [DepTree a]
          deriving (Eq,Show)

isRootMatch :: (Eq a) => a -> DepTree a -> Bool
isRootMatch n Empty = False
isRootMatch n (DLeaf m)   | n==m = True
isRootMatch n (DNode m _) | n==m = True
isRootMatch n _    = False

isNotRootMatch n tree = not $ isRootMatch n tree

buildDepTree :: M.Map GovPos [DepPos] -> DepTree GovPos
buildDepTree gov2deps = go (GovPos 0)
  where go n = case mds of
                   Nothing -> DLeaf n
                   Just ds -> DNode n (map go $ map toGov ds)
              where mds = M.lookup n gov2deps

elimNode :: (Eq a) => a -> DepTree a -> DepTree a
elimNode n Empty                    = Empty
elimNode n (DLeaf m) | n ==m        = Empty
                     | otherwise    = DLeaf m
elimNode n (DNode m ds) | n ==m     = Empty 
                        | otherwise = DNode m (map (elimNode n) $ filter (isNotRootMatch n) ds)

cloneTree :: DepTree a -> DepTree a
cloneTree Empty      = Empty
cloneTree (DLeaf g)    = DLeaf g
cloneTree (DNode g ds) = DNode g (map cloneTree ds)

treeMap :: (a -> b) -> DepTree a -> DepTree b
treeMap f Empty      = Empty
treeMap f (DLeaf g)    = DLeaf (f g)
treeMap f (DNode g ds) = DNode (f g) (map (treeMap f) ds)


nonEmpty :: DepTree a -> DepTree a -> DepTree a
nonEmpty Empty n     = n
nonEmpty n     _     = n

subTree :: (Eq a) => a -> DepTree a -> DepTree a
subTree n Empty = Empty
subTree n (DLeaf g)
  | n==g      = DLeaf g
  | otherwise = Empty
subTree n (DNode g ds)
  | n==g      = DNode g ds
  | otherwise = foldr nonEmpty Empty (map (subTree n) ds)


parse_double :: String -> [Double]
parse_double ds = map read (lines ds)

parse_int :: String -> [Int]
parse_int ds = map read (lines ds)

all_wuids = map (\x -> WUID x) [0..]
loadWordUIDs wuidstr = M.fromList $ zip (map T.pack $ lines wuidstr) all_wuids

instance Functor DepTree where
  fmap = treeMap    -- define fmap for Tree, ad hoc polymorphism


data DepWord = ROOT 
             | A Text
             deriving (Show)

--nodeDepWord :: Maybe Dep -> DepWord
--nodeDepWord Nothing = ROOT
nodeDepWord :: Maybe Dep -> Maybe Text
nodeDepWord Nothing = Nothing
nodeDepWord (Just (Dep x)) = Just x

composeMap a2b b2c a = let f Nothing  = Nothing
                           f (Just b) = M.lookup b b2c
                       in f (M.lookup a a2b)
-- Show??

maybeFun f Nothing = Nothing
maybeFun f (Just x) = (f x)

maybeLookup x2y Nothing  = Nothing
maybeLookup x2y (Just x)  = M.lookup x x2y


{-
foldl' (+) 0 [1,Just 2,3,4] = ! (((!+2)+3)+4)

foldr (+) 0 [1,2,3,4] = Just 2+_ (3+4))

foldl' f 0 [1,2,3,4] = ((Just  f 2) f 3) f 4

foldr f z [] = z 
foldr f z (x:xs) = f x (foldr f z xs)   

foldr nonEmpty Empty (Empty:xs) = nonEmpty Empty (foldr nonEmpty Empty xs)
                              = foldr nonEmpty Empty xs

xs = y : ys
foldr nonEmpty Empty (y:ys) = nonEmpty y (foldr nonEmpty Empty xs) = y

-- HEAD position

f x y ... 

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs  -- tail recursion 

foldl' f z [] = z
foldl' f z (x:xs) = let acc = f z x
                    in acc `seq` foldl' f acc xs  -- tail recursion 


foldl' nonEmpty Empty [] = Empty
foldl' nonEmpty n (n':xs) = foldl' nonEmpty (nonEmpty n n') xs  -- tail recursion 
foldl' nonEmpty n (m:xs') = foldl' nonEmpty (nonEmpty n m ) xs  -- tail recursion 


-- tail recursion = (HEAD = original)


-- let  x :: Num a => a 
        x = g (f y) (h z)
--     x' = h (f y) 
-- 


(+) !x !y = value 
f x y = value

-- recursion scheme
-}

-- foldr ((:) . f) 


findSub :: (Eq a) => a -> DepTree a -> Maybe (DepTree a)
findSub n Empty = Nothing
findSub n (DLeaf g)
  | n==g      = Just (DLeaf g)
  | otherwise = Nothing
findSub n (DNode g ds)
  | n==g      = Just (DNode g ds)
  | otherwise = let xs = catMaybes (map (findSub n) ds)
                in listToMaybe xs
{-
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just a:xs) =  a : catMaybes xs


listToMaybe (catMaybes xs) = listToMaybe ( a : _catMaybes xs )
                           = Just a 

f ( x@(a:as) ) =  case x of
                    (a:_) -> Just a
g (!a,!as)

if b then s1 else s2
ifThenElse (b,s1,s2)


listToMaybe xs = case xs of
                   []    -> Nothing
                   (x:_) -> Just x 
data [] a = []
          | a : [] a



map (findSub n) ds :: [Maybe DepTree]

catMaybes [ Nothing, ... , Just a , Nothing, Just [b], ... ] = [a,b]
catMaybes [ Nothing, ... ] = [] 
-}
  


-- Just for cross-checking for subTree. 
remove x ds = filter (\d -> x/=d) ds
deregister acc (g,d) = let f Nothing   = Nothing
                           f (Just ds) = Just (remove d ds)
                           in M.alter f g acc
--M.Map GovPos [DepPos]
deleteGovKey Nothing  d g2ds = g2ds
deleteGovKey (Just g) d g2ds = deregister g2ds (g,d)
deleteDepKey d          d2g  = M.delete d d2g


{-
jsonstr <- BL.readFile "data/sent.json"
let ej = eitherDecode jsonstr :: Either String DepChunk
let chunk = tryDepChunk ej
let ds =  deps $ head $ sents chunk

sents_deps = fmap deps $ sents chunk
sent_deps = head sents_deps
all_nodess = fmap (fmap $ unDepPos.dep_pos ) sents_deps
all_nodes = head all_nodess

dep2gov = M.fromList $ fmap (\x -> (dep_pos x, gov_pos x)) sent_deps
dep2word = M.fromList $ fmap (\x -> (dep_pos x, dep x)) sent_deps
gov2deps = buildDependentsMap $ fmap (\x -> (gov_pos x, dep_pos x)) sent_deps

all_leaf = filter (\x -> isLeaf x gov2deps) all_nodes
all_paths = map (\x -> toRoot x dep2gov) all_leaf

dep_tree = buildDepTree gov2deps
pruned_tree = removeNode (GovPos 15) gov2deps

print.assert$ Empty == elimNode (GovPos 0) dep_tree -- Removing head should result Empty
print.assert$ dep_tree == elimNode (GovPos 1000) dep_tree -- Removing non-existing node does nothing.
print $ elimNode (GovPos 15) dep_tree


wuidstr <- readFile "../../rnn++/tests/data/words.uid"
wimpstr <- readFile "../../rnn++/tests/data/word_importance"
let wuids = loadWordUIDs wuidstr
let wimps = parse_double wimpstr
word2wuid = loadWordUIDs wuidstr
wuid2score = M.fromList $ zip all_wuids wimps
word2score = composeMap  word2wuid wuid2score

fmap (\x -> (x, maybeFun word2score $ nodeDepWord $ M.lookup (toDep x) dep2word)) dep_tree

--Try another method
node = (GovPos 10)
parent_node = M.lookup (toDep node) dep2gov

subtree1 = elimNode node dep_tree
subtree2 = buildDepTree $ deleteGovKey parent_node (toDep node) gov2deps
print.assert $ subtree1 == subtree2

-- list => map, "fold", concat, concatMap, take, break, scanl` 
-}

-- type FilePath = String 
-- Prelude.readFile :: FilePath -> IO String
-- BL.readFile :: FilePath -> IO BL.ByteString
-- T.IO.readFile :: FilePath -> IO Text

main :: IO ()
main = do
  -- BL.readFile :: FilePath -> IO BL.ByteString (O)
  -- BL.readFile :: FilePath -> BL.ByteString    (X)
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
  print.assert$ remove 3 [1,2,3,4] == [1,2,4]

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
