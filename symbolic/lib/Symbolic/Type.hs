{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Type where

import           Control.Lens              (over, view, _1)
import qualified Data.Array  as A
import qualified Data.Binary as Bi
import qualified Data.ByteString.Lazy as LB
import           Data.Graph
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Data.Maybe                (fromJust)
import           Data.MemoTrie
import           Data.Vector.Storable      (Vector)
--

type Hash = Int

type IndexSymbol = String

type Index = (IndexSymbol,Int,Int)

indexName :: Index -> IndexSymbol
indexName = view _1

data Symbol = Simple String
            | Indexed String [Index]
            deriving (Show, Eq)

isSimple :: Symbol -> Bool
isSimple (Simple _) = True
isSimple _          = False

isIndexed :: Symbol -> Bool
isIndexed (Indexed _ _) = True
isIndexed _             = False

varName :: Symbol -> String
varName (Simple v) = v
varName (Indexed v _) = v

showSym :: Symbol -> String
showSym (Simple str) = str
showSym (Indexed x k) = x ++ "_" ++ concat (map indexName k)

instance HasTrie Symbol where
  data (Symbol :->: b) = SymbolTrie (String :->: b) ((String,[Index]) :->: b)
  
  trie :: (Symbol -> b) -> (Symbol :->: b)
  trie f = SymbolTrie (trie (f . Simple)) (trie (f . uncurry Indexed))

  untrie :: (Symbol :->: b) -> Symbol -> b
  untrie (SymbolTrie s _) (Simple x) = untrie s x
  untrie (SymbolTrie _ i) (Indexed x k) = untrie i (x,k)

  enumerate :: (Symbol :->: b) -> [(Symbol,b)]
  enumerate (SymbolTrie s i) = enum' Simple s `weave` enum' (uncurry Indexed) i

instance Hashable Symbol where
  hashWithSalt :: Hash -> Symbol -> Hash
  hashWithSalt s (Simple str)  = s `hashWithSalt` str
  hashWithSalt s (Indexed x k) = s `hashWithSalt` x `hashWithSalt` k

data Exp a = Zero
           | One
           | Delta Index Index
           | CDelta Index [[Index]] Int
                -- ^ delta for collective index
                -- (collective index, index scheme, partition number (1-based))
           | Val a
           | Var Symbol
           | Add [Hash]
           | Mul [Hash]
           | Fun String [Hash]
           | Sum [Index] Hash
           | Concat Index [Hash]
         deriving (Show,Eq)

isZero :: Exp a -> Bool
isZero Zero = True
isZero _ = False

isOne :: Exp a -> Bool
isOne One = True
isOne _ = False

isDelta :: Exp a -> Bool
isDelta (Delta _ _) = True
isDelta _           = False

isSum :: Exp a -> Bool
isSum (Sum _ _) = True
isSum _         = False

isSumOrConcat :: Exp a -> Bool
isSumOrConcat (Sum _ _)    = True
isSumOrConcat (Concat _ _) = True
isSumOrConcat _            = False



data MExp a = MExp { mexpExp :: Exp a
                   , mexpMap :: HashMap Hash (MExp a)
                   , mexpIdx :: HashSet Index -- IndexSymbol
                   }

getMHash :: (HasTrie a, ?expHash :: Exp a :->: Hash) => MExp a -> Hash
getMHash e = untrie ?expHash (mexpExp e)

data RExp a = RZero
            | ROne
            | RDelta Index Index
            | RCDelta Index [[Index]] Int
            | RVal a
            | RVar Symbol
            | RAdd [RExp a]
            | RMul [RExp a]              
            | RFun String [RExp a]
            | RSum [Index] (RExp a)
            | RConcat Index [RExp a]


mangle :: Double -> [Int]
mangle = map fromIntegral . LB.unpack . Bi.encode

unmangle :: [Int] -> Double
unmangle = Bi.decode . LB.pack . map fromIntegral

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c 

instance HasTrie Double where
  data Double :->: a = DoubleTrie ([Int] :->: a)
  trie f = DoubleTrie $ trie $ f . unmangle
  untrie (DoubleTrie t) = untrie t . mangle

  -- for the time being
  enumerate = error "enumerate of HasTrie instance of Double is undefined"

instance HasTrie a => HasTrie (Exp a) where
  data (Exp a :->: b) = ExpTrie (() :->: b)
                                (() :->: b)
                                ((Index,Index) :->: b)
                                ((Index,[[Index]],Int) :->: b)
                                (a :->: b)
                                (Symbol :->: b)
                                ([Hash] :->: b)     -- ^ for Add
                                ([Hash] :->: b)     -- ^ for Mul
                                ((String,[Hash]) :->: b) -- ^ for Fun
                                (([Index],Hash) :->: b)
                                ((Index,[Hash]) :->: b)
                        
  trie :: (Exp a -> b) -> (Exp a :->: b)
  trie f = ExpTrie (trie (\() -> f Zero))
                   (trie (\() -> f One))
                   (trie (f . uncurry Delta))
                   (trie (f . uncurry3 CDelta))
                   (trie (f . Val))
                   (trie (f . Var))
                   (trie (f . Add))
                   (trie (f . Mul))
                   (trie (f . uncurry Fun))
                   (trie (f . uncurry Sum))
                   (trie (f . uncurry Concat))
           
  untrie :: (Exp a :->: b) -> Exp a -> b
  untrie (ExpTrie z o d cd l v a m f su c) e =
    case e of
      Zero         -> untrie z ()
      One          -> untrie o ()
      Delta i j    -> untrie d (i,j)
      CDelta i is p -> untrie cd (i,is,p)      
      Val n        -> untrie l n
      Var s        -> untrie v s
      Add hs       -> untrie a hs
      Mul hs       -> untrie m hs
      Fun s hs     -> untrie f (s,hs) 
      Sum is h     -> untrie su (is,h)
      Concat i hs  -> untrie c (i,hs)
                                     
  enumerate :: (Exp a :->: b) -> [(Exp a,b)]
  enumerate (ExpTrie z o d cd n v a m f su c) =
    enum' (\()->Zero) z
    `weave`
    enum' (\()->One) o
    `weave`
    enum' (uncurry Delta) d 
    `weave`
    enum' (uncurry3 CDelta) cd
    `weave`
    enum' Val n
    `weave`
    enum' Var v
    `weave`
    enum' Add a
    `weave`
    enum' Mul m
    `weave`
    enum' (uncurry Fun) f
    `weave`
    enum' (uncurry Sum) su
    `weave`
    enum' (uncurry Concat) c
    

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a',b)]
enum' f = fmap (over _1 f) . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)
                  
instance Hashable a => Hashable (Exp a) where
  hashWithSalt :: Hash -> Exp a -> Hash
  hashWithSalt s Zero            = s `hashWithSalt` (0 :: Int)
  hashWithSalt s One             = s `hashWithSalt` (1 :: Int)
  hashWithSalt s (Delta i j)     = s `hashWithSalt` (2 :: Int) `hashWithSalt` i `hashWithSalt` j
  hashWithSalt s (CDelta i iss p)= s `hashWithSalt` (3 :: Int) `hashWithSalt` i `hashWithSalt` iss `hashWithSalt` p
  hashWithSalt s (Val n)         = s `hashWithSalt` (4 :: Int) `hashWithSalt` n
  hashWithSalt s (Var s')        = s `hashWithSalt` (5 :: Int) `hashWithSalt` s'
  hashWithSalt s (Add hs)        = s `hashWithSalt` (6 :: Int) `hashWithSalt` hs
  hashWithSalt s (Mul hs)        = s `hashWithSalt` (7 :: Int) `hashWithSalt` hs
  hashWithSalt s (Fun s' hs)     = s `hashWithSalt` (8 :: Int) `hashWithSalt` s' `hashWithSalt` hs
  hashWithSalt s (Sum is h1)     = s `hashWithSalt` (9 :: Int) `hashWithSalt` is `hashWithSalt` h1
  hashWithSalt s (Concat i hs)   = s `hashWithSalt` (10 :: Int) `hashWithSalt` i `hashWithSalt` hs


exp2RExp :: MExp a -> RExp a
exp2RExp (MExp Zero        _ _) = RZero
exp2RExp (MExp One         _ _) = ROne
exp2RExp (MExp (Delta i j) _ _) = RDelta i j
exp2RExp (MExp (CDelta i is j) _ _) = RCDelta i is j
exp2RExp (MExp (Val n)     _ _) = RVal n
exp2RExp (MExp (Var s)     _ _) = RVar s
exp2RExp (MExp (Add hs)    m _) = RAdd $ map (exp2RExp . flip justLookup m) hs
exp2RExp (MExp (Mul hs)    m _) = RMul $ map (exp2RExp . flip justLookup m) hs
exp2RExp (MExp (Fun s hs)  m _) = RFun s $ map (exp2RExp . flip justLookup m) hs
exp2RExp (MExp (Sum is h1) m _) = let e1 = justLookup h1 m in RSum is (exp2RExp e1)
exp2RExp (MExp (Concat i hs) m _) = RConcat i $ map (exp2RExp . flip justLookup m) hs


daughters :: Exp a -> [Hash]
daughters Zero           = []
daughters One            = []
daughters (Delta _ _)    = []
daughters (CDelta _ _ _) = []
daughters (Val _)        = []
daughters (Var _)        = []
daughters (Add hs)       = hs
daughters (Mul hs)       = hs
daughters (Fun _ hs)     = hs
daughters (Sum _ h1)     = [h1]
daughters (Concat _ hs)  = hs

mkDepEdges :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> [(Hash,Hash)]
mkDepEdges e = let e1 = mexpExp e
                   h1 = untrie ?expHash e1
                   m1 = mexpMap e
                   hs = daughters e1
                   lst = map (h1,) hs
                   lsts = map (mkDepEdges . flip justLookup m1) hs 
               in concat (lst:lsts)

mkDepEdgesNoSum :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> [(Hash,Hash)]
mkDepEdgesNoSum e =
  let e1 = mexpExp e
  in if isSumOrConcat e1
     then []
     else  
       let h1 = untrie ?expHash e1
           m1 = mexpMap e
           hs = daughters e1
           lst = map (h1,) hs
           lsts = map (mkDepEdgesNoSum . flip justLookup m1) hs 
       in concat (lst:lsts)

mkDepGraph :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> (HashMap Hash Vertex,Table Hash, Graph)
mkDepGraph e = let edgs = mkDepEdges e
                   hs = HS.toList . HS.fromList . concatMap (\(i,j) -> [i,j]) $ edgs
                   hmap = HM.fromList (zip hs [1..])
                   n = length hs
                   arr = A.listArray (1,n) hs
                   edgs' :: [(Vertex,Vertex)]
                   edgs' = flip map edgs $ \(i,j) -> let i' = justLookup i hmap; j' = justLookup j hmap; in (i',j')
               in (hmap,arr,buildG (1,n) edgs')

mkDepGraphNoSum :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> (HashMap Hash Vertex,Table Hash, Graph)
mkDepGraphNoSum e =
  let edgs = mkDepEdgesNoSum e
      hs = HS.toList . HS.fromList . concatMap (\(i,j) -> [i,j]) $ edgs
      hmap = HM.fromList (zip hs [1..])
      n = length hs
      arr = A.listArray (1,n) hs
      edgs' :: [(Vertex,Vertex)]
      edgs' = flip map edgs $ \(i,j) -> let i' = justLookup i hmap; j' = justLookup j hmap; in (i',j')
  in (hmap,arr,buildG (1,n) edgs')


data Pos = Pos1 | Pos2 

type IdxPoint = [(IndexSymbol,Int)]

data Args a = Args { varSimple :: HashMap String a
                   , varIndexed :: HashMap String (Vector a) }


type FunctionMap a = HashMap String ([a] -> a)


justLookup :: (Eq k, Hashable k,Show k) => k -> HashMap k v -> v
justLookup h m = case (HM.lookup h m) of
                   Nothing -> error ("justLookup: error in retrieving " ++ show h)   -- this is very unsafe, but we do not have good solution yet.
                   Just v -> v

justLookupL :: (Eq k,Show k,Show v) => k -> [(k,v)] -> v
justLookupL k l = case (lookup k l) of
                    Nothing -> error ("justLookup: error in retrieving " ++ show k ++ " in " ++ show l)
                    Just v -> v
-- fromJust . lookup k


