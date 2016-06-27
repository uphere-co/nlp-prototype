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
  
prettyPrint RZero = "0"
prettyPrint ROne  = "1"
prettyPrint (RVal n) = show n 
prettyPrint (RVar s) = s
prettyPrint (RFun1 s e1) = printf "( %s %s )" s (prettyPrint e1)
prettyPrint (RFun2 s e1 e2) = printf "( %s %s %s )" s (prettyPrint e1) (prettyPrint e2)

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

x = (Var "x", HM.empty)

y = (Var "y", HM.empty)

one = (One, HM.empty)
zero = (Zero, HM.empty)

val n = (Val n, HM.empty)

biop :: (?expHash :: Exp :->: Hash) => Symbol -> ExpMap -> ExpMap -> ExpMap
biop sym (e1,m1) (e2,m2) =
  let h1 = untrie ?expHash e1
      h2 = untrie ?expHash e2
      e = Fun2 sym h1 h2
      m = (HM.insert h1 e1 . HM.insert h2 e2) (m1 `HM.union` m2)
  in (e,m)

add :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap -> ExpMap
add = biop "+"

mul :: (?expHash :: Exp :->: Hash) => ExpMap -> ExpMap -> ExpMap
mul = biop "*"

square :: (?expHash :: Exp :->: Int) => ExpMap -> ExpMap
square e = mul e e 

power :: (?expHash :: Exp :->: Int) => Int -> ExpMap -> ExpMap
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = one
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = square (power (n `div` 2) e) `mul` e


exp1 :: (?expHash :: Exp :->: Int) => ExpMap
exp1 = square (x `add` y)

exp2 :: (?expHash :: Exp :->: Int) => ExpMap
exp2 = power 10 (x `add` y)


expfib' :: (?expHash :: Exp :->: Int) => (Int :->: ExpMap) -> Int -> ExpMap
expfib' t 0 = val 0 -- let e = Val 0; h = hash e in (HM.singleton h e, Val 0)
expfib' t 1 = val 1 -- let e = Val 1; h = hash e in (HM.singleton h e, Val 1)
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add e1 e2

expfib :: (?expHash :: Exp :->: Int) => Int -> ExpMap 
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib



main = do
    let ?expHash = trie hash
    let (e,m) = exp2
        (ex,mx) = x
    putStrLn . prettyPrint . exp2RExp m $ e
    putStrLn . prettyPrint . exp2RExp mx $ ex
    -- putStrLn (prettyPrint (fst exp1))
    -- putStrLn (prettyPrint (fst x))
    -- printf "%x \n" (untrie ?expHash (Val 1))

digraph v = do
    let -- v = expfib 10
        h = untrie ?expHash (fst v)
    let m = HM.insert h (fst v) (snd v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (dotPrint m h) HS.empty
    putStrLn "}"

main' = do
    let ?expHash = trie hash
    
    -- digraph (expfib 10)
    -- digraph exp1
    digraph exp2
