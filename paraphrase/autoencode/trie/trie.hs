{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Arrow
import           Control.Lens              (over, _1)
import           Data.Bits                 (xor)
import           Data.Function             (fix)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
-- import           Data.HashSet              (HashSet)
-- import qualified Data.HashSet        as HS
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
  hashWithSalt s Zero             = trace "hashing Zero" (s `hashWithSalt` (0 :: Int))
  hashWithSalt s One              = trace "hashing One" (s `hashWithSalt` (1 :: Int))
  hashWithSalt s (Val n)          = trace (printf "hashing Val %d" n) (s `hashWithSalt` (2 :: Int) `hashWithSalt` n)
  hashWithSalt s (Var s')         = trace (printf "hashing Var %s" s') (s `hashWithSalt` (3 :: Int) `hashWithSalt` s')
  hashWithSalt s (Fun1 str h1)    = trace (printf "hashing Fun1 %s" str) ( s `hashWithSalt` (4 :: Int) `hashWithSalt` str `hashWithSalt` h1)
  hashWithSalt s (Fun2 str h1 h2) = trace (printf "hashing Fun2 %s %x %x" str h1 h2) ( s `hashWithSalt` (5 :: Int) `hashWithSalt` str `hashWithSalt` h1 `hashWithSalt` h2)

combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
  
prettyPrint Zero = "0"
prettyPrint One  = "1"
prettyPrint (Val n) = show n 
prettyPrint (Var s) = s
prettyPrint (Fun1 s h1) = printf "( %s %x )" s h1
prettyPrint (Fun2 s h1 h2) = printf "( %s %x %x )" s h1 h2

{- 
mkHashMap (?expHash :: Exp :->: Int) => Exp -> HM.Map Int Exp -> HM.Map Int Exp
mkHashMap e m = let h = untrie expHash e
                in case HM.lookup h m of
                     Just _ -> m
                     Nothing ->
                       case e of
                         Fun1 s h1 -> 
                                  
                       

dotPrint m Zero = "0"
dotPrint m One  = "1"
dotPrint m (Val n) = show n 
dotPrint m (Var s) = s
dotPrint m (Fun1 s h1)    = let Just e1 = HM.lookup h1 m
                            in printf "%s -> %x;\n" s h1 <> dotPrint m e1
dotPrint m (Fun2 s h1 h2) = let Just e1 = HM.lookup h1 m
                                Just e2 = HM.lookup h2 m
                            in printf "%s -> %x;\n%s -> %x;" s h1 s h2
-}

add :: (?expHash :: Exp :->: Int) => Exp -> Exp -> Exp
add e1 e2 = Fun2 "+" (untrie ?expHash e1) (untrie ?expHash e2)

mul :: (?expHash :: Exp :->: Int) => Exp -> Exp -> Exp
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



main = do
    let ?expHash = trie hash

    putStrLn (prettyPrint exp1)

    putStrLn (prettyPrint x)
    printf "%x \n" (untrie ?expHash (Val 1))

    -- let expfib = fix (expfib' . trie)
      
    putStrLn " hash for expfib 35"
    let v = expfib 35
    printf "%x \n"  (untrie ?expHash (snd v))
    mapM_ (printf "%x ")  (HM.keys (fst v))

    -- putStrLn (dotPrint exp1)
    

