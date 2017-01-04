import Data.List (sortBy,sort)
import Data.Function (on)

{- 
instance Num Int
instance Ord Int where
class Ord a where
  compare :: a -> a -> Ordering

data Ordering = LT | EQ | GT

instance (Ord a, Ord b) => Ord (a,b) 

-}


lst = [(1,'a'),(3,'d'),(3,'e'),(2,'a'),(0,'b'),(1,'f'),(7,'c'),(-2,'a')]

-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]

{- 
compareDesc x y = case compare x y of
                    LT -> GT
                    EQ -> EQ
                    GT -> LT

compareDesc x y = compare y x 


-- higher order function = function of function

flip :: (a -> b -> c) -> b -> a -> c


id :: a -> a 

($) :: (a -> b) -> a -> b
f $ x = f x 

infixl (precidence) $
infixr

a + b + c = (a + b) + c
 a + (b + c)

f a b = a `f` b

mod 10 3 = 10 `mod` 3 
div
infixl 9 `mod` 

-- f (a b  c) = f $ a b c  

(.) :: (b -> c) -> (a -> b) -> a -> c

-- (f . g) (x) = f (g (x)) 


-}


data Foo = Foo { fooX :: Int
               , fooY :: Double
               , fooZ :: Char }

main :: IO ()
main= do
  putStrLn "hello"
  let x = sort lst
      y = sortBy (flip compare)  lst
      z = sortBy (\(x0,y0) (x1,y1) -> flip compare y0 y1) lst
      w = sortBy (flip compare `on` snd) lst
      -- w' = sortBy (compare `on` fooZ) lstofFoo
  print x
  print y
  print z
  print w


-- on (flip compare) (snd)

-- on :: (b -> b -> Ordering) -> ((a,b) -> b) -> (a,b) -> (a,b) -> Ordering
-- on cmp ext x y = cmp (ext x) (ext y) 

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- y 
