{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  
prettyPrint Zero = "0"
prettyPrint One  = "1"
prettyPrint (Val n) = show n 
prettyPrint (Var s) = s
prettyPrint (Fun1 s h1) = printf "( %s %x )" s h1
prettyPrint (Fun2 s h1 h2) = printf "( %s %x %x )" s h1 h2

                       
dotPrint :: HashMap Int Exp -> Hash -> State (HashSet Int) String
dotPrint m h = do
  s <- get
  let Just e = HM.lookup h m
  case h `HS.member` s of
    True -> return ""
    False -> do
      let (str,hs) = dotPrint' h e
      put (h `HS.insert` s)
      lst <- mapM (dotPrint m) hs
      return (concat (str : lst))
      -- in str ++ concatMap dotPrint m (hs `HS.union` s) 

dotPrint' h Zero           = (printf "x%x [label=\"0\"];\n" h,[])
dotPrint' h One            = (printf "x%x [label=\"1\"];\n" h,[])
dotPrint' h (Val n)        = (printf "x%x [label=\"%d\"];\n" h n ,[])
dotPrint' h (Var s)        = (printf "x%x [label=\"%s\"];\n" h s,[])
dotPrint' h (Fun1 s h1)    = (printf "x%x [label=\"%s\"];\n%s -> x%x;\n" h s h h1,[h1]) --  ++ dotPrint ms h1
dotPrint' h (Fun2 s h1 h2) = (printf "x%x [label=\"%s\"];\nx%x -> x%x;\nx%x -> x%x;\n" h s h h1 h h2,[h1,h2])

add :: (?expHash :: Exp :->: Hash) => Exp -> Exp -> Exp
add e1 e2 = Fun2 "+" (untrie ?expHash e1) (untrie ?expHash e2)

mul :: (?expHash :: Exp :->: Hash) => Exp -> Exp -> Exp
mul e1 e2 = Fun2 "*" (untrie ?expHash e1) (untrie ?expHash e2)

x = Var "x"
y = Var "y"

square :: (?expHash :: Exp :->: Int) => Exp -> Exp
square e = mul e e 


power :: (?expHash :: Exp :->: Int) => Int -> Exp -> Exp
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = One
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = square (power (n `div` 2) e) `mul` e


exp1 :: (?expHash :: Exp :->: Int) => Exp
exp1 = square (x `add` y)

exp2 :: (?expHash :: Exp :->: Int) => Exp
exp2 = power 10 (x `add` y)

expfib' :: (?expHash :: Exp :->: Int) => (Int :->: (HashMap Int Exp, Exp)) -> Int -> (HashMap Int Exp,Exp)
expfib' t 0 = let e = Val 0; h = hash e in (HM.singleton h e, Val 0)
expfib' t 1 = let e = Val 1; h = hash e in (HM.singleton h e, Val 1)
expfib' t n = let (s1,e1) = untrie t (n-1)
                  (s2,e2) = untrie t (n-2)
                  e = add e1 e2
                  h = hash e
              in (HM.insert h e (HM.union s1 s2), e)


--  let e = add (untrie t (s,(n-1))) (untrie t (s,(n-2))); h = hash e in (HS.insert h s, e)



expfib :: (?expHash :: Exp :->: Int) => Int -> (HashMap Int Exp, Exp)
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib



main' = do
    let ?expHash = trie hash

    putStrLn (prettyPrint exp1)

    putStrLn (prettyPrint x)
    printf "%x \n" (untrie ?expHash (Val 1))

    -- let expfib = fix (expfib' . trie)


main = do
    let ?expHash = trie hash  
    let v = expfib 10
        h = untrie ?expHash (snd v)
    let m = fst v 
    putStrLn "digraph G {"
    putStrLn $ evalState (dotPrint m h) HS.empty
    putStrLn "}"

