-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens (over, _1)
import Data.Function (fix)
-- import Data.Hashable
import Data.MemoTrie
-- import GHC.Generics (Generic)

type Symbol = String

data Exp = Zero
         | One
         | Var Symbol
         | Fun1 Symbol Exp
         | Fun2 Symbol Exp Exp
         -- | Fun3 Symbol Exp Exp Exp
         deriving (Show,Eq) -- Generic

instance HasTrie Exp where
  data (Exp :->: b) = ExpTrie (() :->: b)
                              (() :->: b)
                              (Symbol :->: b)
                              ((Symbol,Exp) :->: b)
                              ((Symbol,Exp,Exp) :->: b)
  trie f = ExpTrie (trie (\() -> f Zero)) (trie (\() -> f One)) (trie (f . Var))
                   (trie (\(s,e)-> f (Fun1 s e))) (trie (\(s,e1,e2)-> f (Fun2 s e1 e2)))
  untrie (ExpTrie z o v f1 f2) e = case e of
                                     Zero         -> untrie z ()
                                     One          -> untrie o ()
                                     Var s        -> untrie v s
                                     Fun1 s e     -> untrie f1 (s,e)
                                     Fun2 s e1 e2 -> untrie f2 (s,e1,e2)
  enumerate (ExpTrie z o v f1 f2) = enum' (\()->Zero) z
                                    `weave`
                                    enum' (\()->One) o
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
                  
add = Fun2 "+"
mul = Fun2 "*"

x = Var "x"
y = Var "y"

prettyPrint Zero = "0"
prettyPrint One  = "1"
prettyPrint (Var s) = s
prettyPrint (Fun1 s e) = "( " ++ s ++ " " ++ prettyPrint e ++ " )"
prettyPrint (Fun2 s e1 e2) = "( " ++ s ++ " " ++ prettyPrint e1 ++ " " ++ prettyPrint e2 ++ " )"


square :: Exp -> Exp
square e = e `mul` e


power :: Int -> Exp -> Exp
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = One
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = square (power (n `div` 2) e) `mul` e

-- cseDetector :: HashMap 


exp1 = square (x `add` y)
exp2 = power 10 (x `add` y)


fib' :: (Int :->: Int) -> Int -> Int
fib' t 0 = 0 
fib' t 1 = 1
fib' t n = untrie t (n-1) + untrie t (n-2)

main = do
    mapM_ (putStrLn . prettyPrint) [ exp1, exp2 ]

    -- let t = trie fib
    --    fib = fib' t 
    -- let fib = fib' (trie fib)
    let fib = fix (fib' . trie)
    
    print (fib 50)
    
