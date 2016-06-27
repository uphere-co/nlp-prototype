{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Type where

import           Control.Arrow
import           Control.Lens              (over, _1)
import           Control.Monad.Trans.State
import           Data.Bits                 (xor)
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Text.Printf
--
import Debug.Trace

type Symbol = String

type Hash = Int

data Exp = Zero
          | One
          | Val Int
          | Var Symbol
          | Fun1 Symbol Hash
          | Fun2 Symbol Hash Hash
          -- deriving (Show,Eq) -- Generic

type ExpMap = (Exp,HashMap Hash Exp)

data RExp = RZero
          | ROne
          | RVal Int
          | RVar Symbol
          | RFun1 Symbol RExp
          | RFun2 Symbol RExp RExp

instance HasTrie Exp where
  data (Exp :->: b) = ExpTrie (() :->: b)
                               (() :->: b)
                               (Int :->: b)
                               (Symbol :->: b)
                               ((Symbol,Hash) :->: b)
                               ((Symbol,Hash,Hash) :->: b)
  trie :: (Exp -> b) -> (Exp :->: b)
  trie f = ExpTrie (trie (\() -> f Zero))
                    (trie (\() -> f One))
                    (trie (f . Val))
                    (trie (f . Var))
                    (trie (\(s,e)-> f (Fun1 s e)))
                    (trie (\(s,e1,e2)-> f (Fun2 s e1 e2)))
           
  untrie :: (Exp :->: b) -> Exp -> b
  untrie (ExpTrie z o l v f1 f2) e =
    case e of
      Zero         -> untrie z ()
      One          -> untrie o ()
      Val n        -> untrie l n
      Var s        -> untrie v s
      Fun1 s e     -> untrie f1 (s,e)
      Fun2 s e1 e2 -> untrie f2 (s,e1,e2)
                                     
  enumerate :: (Exp :->: b) -> [(Exp,b)]
  enumerate (ExpTrie z o n v f1 f2) =
    enum' (\()->Zero) z
    `weave`
    enum' (\()->One) o
    `weave`
    enum' Val n
    `weave`
    enum' Var v
    `weave`
    enum' (\(s,e)->Fun1 s e) f1
    `weave`
    enum' (\(s,e1,e2)->Fun2 s e1 e2) f2




enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a',b)]
enum' f = fmap (over _1 f) . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)
                  
instance Hashable Exp where
  hashWithSalt :: Int -> Exp -> Int
  hashWithSalt s Zero            = s `hashWithSalt` (0 :: Int)
  hashWithSalt s One             = s `hashWithSalt` (1 :: Int)
  hashWithSalt s (Val n)         = s `hashWithSalt` (2 :: Int) `hashWithSalt` n
  hashWithSalt s (Var s')        = s `hashWithSalt` (3 :: Int) `hashWithSalt` s'
  hashWithSalt s (Fun1 s' h1)    = s `hashWithSalt` (4 :: Int) `hashWithSalt` s' `hashWithSalt` h1
  hashWithSalt s (Fun2 s' h1 h2) = s `hashWithSalt` (5 :: Int) `hashWithSalt` s' `hashWithSalt` h1 `hashWithSalt` h2

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

exp2RExp :: HashMap Hash Exp -> Exp -> RExp
exp2RExp m Zero           = RZero
exp2RExp m One            = ROne
exp2RExp m (Val n)        = RVal n
exp2RExp m (Var s)        = RVar s
exp2RExp m (Fun1 s h1)    = let e1 = case HM.lookup h1 m of
                                       Nothing -> error " fun1 "
                                       Just e -> e
                            in RFun1 s (exp2RExp m e1)
exp2RExp m (Fun2 s h1 h2) = let e1 = case HM.lookup h1 m of
                                            Nothing -> error " fun2 "
                                            Just e -> e
                                e2 = case HM.lookup h2 m of
                                            Nothing -> error "fun2 2"
                                            Just e -> e
                            in RFun2 s (exp2RExp m e1) (exp2RExp m e2)
