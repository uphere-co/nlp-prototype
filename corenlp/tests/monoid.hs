{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import Data.Char  (toUpper, toLower)
import Data.Monoid  ((<>), All(..), Any(..),First(..), Last(..))

{- 
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  
  mappend = (<>)
-}

{-
instance Monoid Int where
  mempty = 0
  mappend = (+)

instance Monoid Int where
  mempty = 1
  mappend = (*)

instance Monoid [a] where
  mempty = []
  mappend = (++)
-}

{-
(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

template <type A, type B, type C>
  flip( function<A,B,C> f, A a, B b) = ...

template <type T> mappend (T x, T y )
-}

newtype Addable = A Int

instance Show Addable where
  show (A n) = show n

instance Monoid Addable where
  mempty = A 0
  mappend (A x) (A y) = A (x+y)

newtype Multipliable = M Int

instance Show Multipliable where
  show (M n) = show n

instance Monoid Multipliable where
  mempty = M 1
  mappend (M x) (M y) = M (x*y)

-- monoid law

-- m <> 0 = m
-- 0 <> m = m

{- 
data A = A X Y
       | B X
-}

{-
instance Monoid (AddBool) where
  mempty = False
  mappend = (||)

AddBool = Any in Data.Monoid

instance Monoid (MulBool)
  mempty = True
  mappend = (&&)

MulBool = All in Data.Monoid

-}


main' = do
  print ([1,2,3] <> [4,5])
  print (A 1 <> A 3 <> A 5 )

  print (M 1 <> M 3 <> M 5)


  print (M 1 <> mempty)   -- surprise can happen

  print (All True <> All True <> All False)
  print (Any True <> Any True <> Any False)


  print (mconcat [All True, All True, All False])

 -- Any True True True False
  -- let f n = Any (n > 5)
  {- 
  f a :: *
  g a b :: *      a  
  f :: * -> * = p    
  g :: * -> * -> * 
  -}
  
  print $ mconcat (map (Any . (>5)) [1,3,6,2,5,1,1])

  print $ mconcat (map (All . (>5)) [1,3,6,2,5,1,1])

{- 

  (+)

  foldr (+) 0

-}



-- Function composition as monoidal operator
--
-- (String -> String


instance Monoid (String -> String) where
  mempty = id
  mappend f g = f . g 


f1 = \x -> (x ++ " ABCDE ")
f2 = \x -> ("abcde " ++ x )

f3 = map toUpper
f4 = map toLower


say :: String -> String -> String
say x = (x  ++)

{- 
instance Monoid (Maybe a) where
  mempty = Nothing
  mappend (Just x) (Just y) = Just x
  mappend Nothing (Just y) = Just y
  mappend (Just x) Nothing = Just x
  mappend Nothing Nothing = Nothing
-}

{-

class (Constraint a) => ClassDef a where

f :: (Constraint a) => a -> b -> c

instance (Monoid m) => Monoid (Maybe m) where
  mempty = Nothing
  mappend (Just x) (Just y) = Just (x <> y)
  mappend Nothing (Just y) = Just y
  mappend (Just x) Nothing = Just x
  mappend Nothing Nothing = Nothing
-}

-- Applicative  -- (*)
-- Alternative  -- (+)


main :: IO ()
main = do
  print (mconcat [Nothing , Just "a", Nothing, Just "b", Just "c", Nothing])
  print (mconcat (map First [Nothing, Just "a", Nothing, Just "b", Just "c", Nothing]))
  print (mconcat (map Last [Nothing, Just "a", Nothing, Just "b", Just "c", Nothing]))

  
  print ((f1 . f2 . f3 . f4) "Ian")

  print (mconcat [f1,f2,f3,f4] "Ian")

  putStrLn (mconcat [ say "hi\n"
                    , say "yo\n"
                    , say "yo Doyoung\n"
                    , say "let's do it\n" ] "")

  print (mconcat ([] :: [String->String]) "Ian")

  --         abc          def
  let x = (say "abc") <> (say "def")


      y = x <> (say "hij")

      z = (say "12") <> y 

  print (z "")

{- 
-- x = abc . def 
-- y = x . hij = abc . def . hij
-- z = 12 . y = 12 . abc . def . hij
-- z "" = (12 . abc . def . hij) ""
        = (12 (abc (def (hij ""))))
        = (12 (abc (def  "hij")))
        = 12 (abc "defhij")
        = 12 "abcdefhij"
        = "12abcdefhij"
   -}       

{- 





-}
