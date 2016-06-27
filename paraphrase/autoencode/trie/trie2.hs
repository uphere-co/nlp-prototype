{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens (over, _1)
import Data.Bits (xor)
import Data.Function (fix)
import Data.Hashable
import Data.MemoTrie
import Text.Printf
--
import Debug.Trace

type Symbol = String

type Hash = Int

data Exp' = Zero
          | One
          | Val Int
          | Var Symbol
          | Fun1 Symbol Hash
          | Fun2 Symbol Hash Hash
          -- deriving (Show,Eq) -- Generic

-- data Exp = Exp { expHash :: Int
--                , expMap :: Int :->: Exp' }
           -- deriving (Show)


instance HasTrie Exp' where
  data (Exp' :->: b) = Exp'Trie (() :->: b)
                               (() :->: b)
                               (Int :->: b)
                               (Symbol :->: b)
                               ((Symbol,Hash) :->: b)
                               ((Symbol,Hash,Hash) :->: b)
  trie :: (Exp' -> b) -> (Exp' :->: b)
  trie f = Exp'Trie (trie (\() -> f Zero))
                    (trie (\() -> f One))
                    (trie (f . Val))
                    (trie (f . Var))
                    (trie (\(s,e)-> f (Fun1 s e)))
                    (trie (\(s,e1,e2)-> f (Fun2 s e1 e2)))
           
  untrie :: (Exp' :->: b) -> Exp' -> b
  untrie (Exp'Trie z o l v f1 f2) e =
    case e of
      Zero         -> untrie z ()
      One          -> untrie o ()
      Val n        -> untrie l n
      Var s        -> untrie v s
      Fun1 s e     -> untrie f1 (s,e)
      Fun2 s e1 e2 -> untrie f2 (s,e1,e2)
                                     
  enumerate :: (Exp' :->: b) -> [(Exp',b)]
  enumerate (Exp'Trie z o n v f1 f2) =
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

{- 
instance HasTrie Exp where
  newtype (Exp :->: b) = ExpTrie (Exp' :->: b)
  trie f = ExpTrie (trie (f . embed))
  untrie (ExpTrie f) (Exp _ x) = untrie f x
  enumerate :: (Exp :->: b) -> [(Exp,b)]
  enumerate (ExpTrie f) = enum' embed f
-}

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a',b)]
enum' f = fmap (over _1 f) . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)
                  


instance Hashable Exp' where
  hashWithSalt :: Int -> Exp' -> Int
  hashWithSalt s Zero             = trace "hashing Zero" (s `combine` 0)
  hashWithSalt s One              = trace "hashing One" (s `combine` 1)
  hashWithSalt s (Val n)          = trace ("hashing Val " ++ show n) (s `combine` 2 `combine` n)
  hashWithSalt s (Var s')         = trace ("hashing Var " ++ s') (s `combine` 3 `hashWithSalt` s')
  hashWithSalt s (Fun1 str h1)    = trace ("hashing Fun1 " ++ str) ( s `combine` 4 `hashWithSalt` str `hashWithSalt` h1)
  hashWithSalt s (Fun2 str h1 h2) = trace ("hashing Fun2 " ++ str ++ show h1 ++ show h2) ( s `combine` 5 `hashWithSalt` str `hashWithSalt` h1 `hashWithSalt` h2)

{-
instance Hashable Exp where
  hashWithSalt s (Exp h e) = hashWithSalt s h
-}

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
  
prettyPrint' Zero = "0"
prettyPrint' One  = "1"
prettyPrint' (Val n) = show n 
prettyPrint' (Var s) = s
prettyPrint' (Fun1 s h1) = printf "( %s %x )" s h1
prettyPrint' (Fun2 s h1 h2) = printf "( %s %x %x )" s h1 h2

-- prettyPrint (Exp _ e) = prettyPrint' e

{- embed :: Exp' -> Exp
embed e' = Exp (hash e') e'
-}
-- Exp (trie hash)

add h e1 e2 = Fun2 "+" (untrie h e1) (untrie h e2)
mul h e1 e2 = Fun2 "*" (untrie h e1) (untrie h e2)

x = Var "x"
y = Var "y"

square :: (Exp' :->: Int) -> Exp' -> Exp'
square h e = mul h e e

power :: (Exp' :->: Int) -> Int -> Exp' -> Exp'
power h n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = One
  | n `mod` 2 == 0 = square h (power h (n `div` 2) e)
  | otherwise      = mul h (square h (power h (n `div` 2) e)) e

-- cseDetector :: HashMap 

exp1 h = square h (add h x y)
exp2 h = power h 10 (add h x y)

{- 
expfib 0 = Val 0
expfib 1 = Val 1
expfib n = add (expfib (n-1)) (expfib (n-2))
-}

fib' :: (Int :->: Int) -> Int -> Int
fib' t 0 = 0 
fib' t 1 = 1
fib' t n = trace ("fib' n = " ++ show n) ( untrie t (n-1) + untrie t (n-2) )



testfib = do
    -- let t = trie fib
    --    fib = fib' t 
    -- let fib = fib' (trie fib)
    let fib = fix (fib' . trie)
    print (fib 50)


main = do
    -- testfib
    -- mapM_ (putStrLn . prettyPrint) [ exp1, exp2 ]

    -- mapM_ (printf "%x\n" . hash) [exp1, exp2]

    -- expfib spits out very very large expression. cannot go over 40
    -- (putStrLn . prettyPrint) ( expfib 30 )

    -- also this hash calculation takes forever
    -- printf "%x\n" . hash $ expfib 35

    -- let x@(Exp t y) = expfib 35
    
    -- printf "%x\n" (untrie t y)

    putStrLn (prettyPrint' (exp1 (trie hash))) -- (untrie t y)
    -- putStrLn . prettyPrint $ exp1

    
