{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Type where

import           Control.Lens              (over, _1)
import qualified Data.Binary as Bi
import qualified Data.ByteString.Lazy as LB
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import           Data.Maybe                (fromJust)
import           Data.MemoTrie
--

type Hash = Int

type Index = String

data Symbol = Simple String
            | Indexed String [Index]
            deriving (Show, Eq)

showSym (Simple str) = str
showSym (Indexed x k) = x ++ "_" ++ concat k

instance HasTrie Symbol where
  data (Symbol :->: b) = SymbolTrie (String :->: b) ((String,[Index]) :->: b)
  
  trie :: (Symbol -> b) -> (Symbol :->: b)
  trie f = SymbolTrie (trie (f . Simple)) (trie (f . uncurry Indexed))

  untrie :: (Symbol :->: b) -> Symbol -> b
  untrie (SymbolTrie s i) (Simple x) = untrie s x
  untrie (SymbolTrie s i) (Indexed x k) = untrie i (x,k)

  enumerate :: (Symbol :->: b) -> [(Symbol,b)]
  enumerate (SymbolTrie s i) = enum' Simple s `weave` enum' (uncurry Indexed) i

instance Hashable Symbol where
  hashWithSalt :: Hash -> Symbol -> Hash
  hashWithSalt s (Simple str)  = s `hashWithSalt` str
  hashWithSalt s (Indexed x k) = s `hashWithSalt` x `hashWithSalt` k

data Exp a = Zero
           | One
           | Delta Index Index
           | Val a
           | Var Symbol
           | Fun1 String Hash
           | Fun2 String Hash Hash
           | Sum [Index] Hash
         deriving (Show,Eq)

isDelta :: Exp a -> Bool
isDelta (Delta _ _) = True
isDelta _           = False

data MExp a = MExp { mexpExp :: Exp a
                   , mexpMap :: HashMap Hash (MExp a)
                   , mexpIdx :: HashSet Index
                   }

data RExp a = RZero
            | ROne
            | RDelta Index Index
            | RVal a
            | RVar Symbol
            | RFun1 String (RExp a)
            | RFun2 String (RExp a) (RExp a)
            | RSum [Index] (RExp a)

mangle :: Double -> [Int]
mangle = map fromIntegral . LB.unpack . Bi.encode

unmangle :: [Int] -> Double
unmangle = Bi.decode . LB.pack . map fromIntegral

instance HasTrie Double where
  data Double :->: a = DoubleTrie ([Int] :->: a)
  trie f = DoubleTrie $ trie $ f . unmangle
  untrie (DoubleTrie t) = untrie t . mangle

instance HasTrie a => HasTrie (Exp a) where
  data (Exp a :->: b) = ExpTrie (() :->: b)
                                (() :->: b)
                                ((Index,Index) :->: b)
                                (a :->: b)
                                (Symbol :->: b)
                                ((String,Hash) :->: b)
                                ((String,Hash,Hash) :->: b)
                                (([Index],Hash) :->: b)
  trie :: (Exp a -> b) -> (Exp a :->: b)
  trie f = ExpTrie (trie (\() -> f Zero))
                   (trie (\() -> f One))
                   (trie (f . uncurry Delta))
                   (trie (f . Val))
                   (trie (f . Var))
                   (trie (f . uncurry Fun1))
                   (trie (\(s,e1,e2)-> f (Fun2 s e1 e2)))
                   (trie (f . uncurry Sum))
           
  untrie :: (Exp a :->: b) -> Exp a -> b
  untrie (ExpTrie z o d l v f1 f2 su) e =
    case e of
      Zero         -> untrie z ()
      One          -> untrie o ()
      Delta i j    -> untrie d (i,j)
      Val n        -> untrie l n
      Var s        -> untrie v s
      Fun1 s e1    -> untrie f1 (s,e1)
      Fun2 s e1 e2 -> untrie f2 (s,e1,e2)
      Sum is e1    -> untrie su (is,e1)
                                     
  enumerate :: (Exp a :->: b) -> [(Exp a,b)]
  enumerate (ExpTrie z o d n v f1 f2 su) =
    enum' (\()->Zero) z
    `weave`
    enum' (\()->One) o
    `weave`
    enum' (uncurry Delta) d 
    `weave`
    enum' Val n
    `weave`
    enum' Var v
    `weave`
    enum' (uncurry Fun1) f1
    `weave`
    enum' (\(s,e1,e2)->Fun2 s e1 e2) f2
    `weave`
    enum' (uncurry Sum) su

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
  hashWithSalt s (Val n)         = s `hashWithSalt` (3 :: Int) `hashWithSalt` n
  hashWithSalt s (Var s')        = s `hashWithSalt` (4 :: Int) `hashWithSalt` s'
  hashWithSalt s (Fun1 s' h1)    = s `hashWithSalt` (5 :: Int) `hashWithSalt` s' `hashWithSalt` h1
  hashWithSalt s (Fun2 s' h1 h2) = s `hashWithSalt` (6 :: Int) `hashWithSalt` s' `hashWithSalt` h1 `hashWithSalt` h2
  hashWithSalt s (Sum is h1)     = s `hashWithSalt` (7 :: Int) `hashWithSalt` is `hashWithSalt` h1


justLookup :: (Eq k, Hashable k) => k -> HashMap k v -> v
justLookup h m = fromJust (HM.lookup h m)  -- this is very unsafe, but we do not have good solution yet.

exp2RExp :: MExp a -> RExp a
exp2RExp (mexpExp -> Zero)         = RZero
exp2RExp (mexpExp -> One)          = ROne
exp2RExp (mexpExp -> Delta i j)    = RDelta i j
exp2RExp (mexpExp -> Val n)        = RVal n
exp2RExp (mexpExp -> Var s)        = RVar s
exp2RExp (MExp (Fun1 s h1) m _)    = let e1 = justLookup h1 m in RFun1 s (exp2RExp e1)
exp2RExp (MExp (Fun2 s h1 h2) m _) = let e1 = justLookup h1 m; e2 = justLookup h2 m
                                     in RFun2 s (exp2RExp e1) (exp2RExp e2)
exp2RExp (MExp (Sum is h1) m _)    = let e1 = justLookup h1 m in RSum is (exp2RExp e1)


daughters :: Exp a -> [Hash]
daughters Zero           = []
daughters One            = []
daughters (Delta i j)    = []
daughters (Val n)        = []
daughters (Var s)        = []
daughters (Fun1 s h1)    = [h1]
daughters (Fun2 s h1 h2) = [h1,h2]
daughters (Sum is h1)    = [h1]
 

data Pos = Pos1 | Pos2 

data TDelta = TDelta2 Index Index
            | TDelta1 Index Int

data EExp a = EVal a
            | EDelta TDelta            
            | EAdd a TDelta
            | EAdd2 TDelta TDelta
            | EMul a TDelta
            | EMul2 TDelta TDelta


