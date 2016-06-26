{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens (over, _1)
import Data.Bits (xor)
import Data.Function (fix)
import Data.Hashable
import Data.MemoTrie
import Text.Printf

type Symbol = String

data Exp' = Zero
          | One
          | Val Int
          | Var Symbol
          | Fun1 Symbol Exp
          | Fun2 Symbol Exp Exp
          -- deriving (Show,Eq) -- Generic

data Exp = Exp (Exp' :->: Int) Exp'
           -- deriving (Show)

instance HasTrie Exp' where
  data (Exp' :->: b) = Exp'Trie (() :->: b)
                               (() :->: b)
                               (Int :->: b)
                               (Symbol :->: b)
                               ((Symbol,Exp) :->: b)
                               ((Symbol,Exp,Exp) :->: b)
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

instance HasTrie Exp where
  newtype (Exp :->: b) = ExpTrie (Exp' :->: b)
  trie f = ExpTrie (trie (f . Exp (trie hash)))
  untrie (ExpTrie f) (Exp _ x) = untrie f x
  enumerate :: (Exp :->: b) -> [(Exp,b)]
  enumerate (ExpTrie f) = enum' embed f

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a',b)]
enum' f = fmap (over _1 f) . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)
                  

instance Hashable Exp' where
  hashWithSalt :: Int -> Exp' -> Int
  hashWithSalt s Zero             = s `combine` 0
  hashWithSalt s One              = s `combine` 1
  hashWithSalt s (Val n)          = s `combine` 2 `combine` n
  hashWithSalt s (Var s')         = s `combine` 3 `hashWithSalt` s'
  hashWithSalt s (Fun1 str e1)    = s `combine` 4 `hashWithSalt` str `hashWithSalt` e1
  hashWithSalt s (Fun2 str e1 e2) = s `combine` 5 `hashWithSalt` str `hashWithSalt` e1 `hashWithSalt` e2

instance Hashable Exp where
  hashWithSalt s (Exp _ e) = hashWithSalt s e

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
  
prettyPrint' Zero = "0"
prettyPrint' One  = "1"
prettyPrint' (Val n) = show n 
prettyPrint' (Var s) = s
prettyPrint' (Fun1 s e) = "( " ++ s ++ " " ++ prettyPrint e ++ " )"
prettyPrint' (Fun2 s e1 e2) = "( " ++ s ++ " " ++ prettyPrint e1 ++ " " ++ prettyPrint e2 ++ " )"

prettyPrint (Exp _ e) = prettyPrint' e

embed :: Exp' -> Exp
embed = Exp (trie hash)

add e1 e2 = embed (Fun2 "+" e1 e2)
mul e1 e2 = embed (Fun2 "*" e1 e2)

x = embed (Var "x")
y = embed (Var "y")

square :: Exp -> Exp
square e = e `mul` e

power :: Int -> Exp -> Exp
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = embed One
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = square (power (n `div` 2) e) `mul` e

-- cseDetector :: HashMap 

exp1 = square (x `add` y)
exp2 = power 10 (x `add` y)

expfib 0 = embed (Val 0)
expfib 1 = embed (Val 1)
expfib n = add (expfib (n-1)) (expfib (n-2))


fib' :: (Int :->: Int) -> Int -> Int
fib' t 0 = 0 
fib' t 1 = 1
fib' t n = untrie t (n-1) + untrie t (n-2)



testfib = do
    -- let t = trie fib
    --    fib = fib' t 
    -- let fib = fib' (trie fib)
    let fib = fix (fib' . trie)
    print (fib 50)

main = do
    mapM_ (putStrLn . prettyPrint) [ exp1, exp2 ]

    mapM_ (printf "%x\n" . hash) [exp1, exp2]

    -- expfib spits out very very large expression. cannot go over 40
    -- (putStrLn . prettyPrint) ( expfib 30 )

    -- also this hash calculation takes forever
    printf "%x\n" . hash $ expfib 35 

    
