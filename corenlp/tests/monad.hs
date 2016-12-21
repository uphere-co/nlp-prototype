--- {-# LANGUAGE DeriveFunctor #-}
---  {-# LANGUAGE KindSignatures #-}


-- import Control.Monad (bind)
{- 
class Show (a :: *) where
  show :: a -> String
-}

data Test = Test

instance Show Test where
  show Test = "Test"


combineshow :: (Show a, Show b) => a ->  b ->  String
combineshow x y = show x ++ ":" ++ show y


 {- 
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

-- higher-kinded polymorphism

-- g should be applied to arbitrary f as Functor with fmap: parametric polymorphism
g :: (Functor f) => f a -> f (f a) -> f a
--- g = fmap .. 

template< template f, x> f<x> g( f<x>&, f< f<x>  >& )

-}

{- 
data (a,b) = Inter a b
data (,) a b 

(,) :: * -> * -> *
(,,):: * -> * -> * -> *
-}

-- higher-kind 

{-
       fmap g 
  f a   --->   f b 

   ^             ^
 f |             | f

   a    --->     b
         g

-}


data Tree a = Leaf a | Bin (Tree a) (Tree a)

            -- deriving Functor
{-
-- Error!

instance Show (Tree a) where
  show (Leaf a) = "Leaf " ++ show a
-}


instance (Show a) => Show (Tree a) where
  show (Leaf a) = "Leaf " ++ show a
  show (Bin t1 t2) = "Bin (" ++ show t1 ++ ") (" ++ show t2 ++ ")" 


-- kind

-- data type which can have value : kind *
-- 3 :: (Int :: *)

-- Int has kind * 
-- Test :: Test
-- Test has kind *

-- Tree 

-- (Tree a) :: *
-- a :: p
-- Tree :: p -> *
-- a :: * 




treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Bin t1 t2) = Bin (treeMap f t1) (treeMap f t2)


instance Functor Tree where
  fmap = treeMap    -- define fmap for Tree, ad hoc polymorphism


test_tree :: Tree Int
test_tree = Bin (Leaf 1) (Bin (Leaf 2) (Leaf 3)) 

{-
Tree Int -> Tree Int
(\x -> x+1)
Tree Int -> Tree String
(\x -> show x ++ "!") 
-}


-- Functor
-- Applicative
-- Monad

-- Monad < Applicative < Functor
--

{-

-- Type theory

class (Functor m) => Monad (m :: * -> *) where
  return :: a -> m a   -- unit
  bind :: (a -> m b) -> m a -> m b
  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) = flip bind 

  id :: a -> a


  -- f(x=3) =  (x=3) + 4  
  print (id 3)

-}


-- (m, unit, join)


-- bind = flip (>>=)

join :: (Monad m) => m (m x) -> m x
join mmx = (>>=) mmx id    -- bind id mmx

{-
a = m x
b = x


bind :: (a -> m b) -> m a -> m b


id :: x -> x 


a -> m b = x -> x

--> therefore a = m x, b = x

bind id :: m (m x) -> m x

-}



-- 1 + 2 = (+) 1 2

-- f x y = x `f` y


{-
instance Monad Maybe where
  return = Just
  bind f Nothing = Nothing
  bind f (Just x) = f x
-}
  
  
-- return = injection
-- bind 


-- side effect ()

-- a -> (a)  
{-
fmap 
    (a) ---->  (b) 


     a  ---->   b

bind

fmap                      ? = join 
     (a)  -----> ((b)) ----> (b) 



      a  -----> (b)

-}


-- Maybe == optional
-- Nothing ==  null
-- null check

-- Maybe a =  ( a or side effect failure)
{-
                           result of   result of 
          this calc         prev          this
bind' :: (a -> Maybe b) -> Maybe a -> Maybe b


a 
if( a ) {
  b = f (a);
}

-}




main' = do
  putStrLn (combineshow 1 "abc")
  print test_tree
  print (fmap (\x->x+1) test_tree)
  let t2 = fmap (\x->x+1) test_tree
  print (fmap (\x -> show x ++ "!") t2)

  print (fmap (\x -> show x ++ "!") (fmap (\x->x+1) test_tree))
  print (fmap ((\x -> show x ++ "!") . (\x->x+1)) test_tree)

  -- Functor axiom (originated from Category theory)
  -- (fmap f . fmap g) = fmap (f . g)
  -- fmap :: C1 -> C2


  -- Functor instance is unique for a given type.
  -- Girard-Reynold isomorphism

  -- generic 
  -- f :: forall a. a -> a
  -- f= id
  -- g :: a -> b -> a 
  -- g = const
  -- const x _ = x 
  
  -- (+1) :: Int -> Int

  print (join (Just (Just 3)))


  let divideBy y 0 = Nothing
      divideBy y x = Just (y/x)


  print (9 `divideBy` 3)
  -- print (bind (flip divideBy 2) (9 `divideBy` 0))
  print ((9 `divideBy` 3) >>= (\x -> x `divideBy` 7) >>= (\y -> return (y + 1)) >>= (\z -> z `divideBy` 3))
  
  print (9 `divideBy` 3 >>= \x ->
         x `divideBy` 7 >>= \y ->
         return (y + 1) >>= \z ->
         z `divideBy` 3)


  print (do
            x <- 9 `divideBy` 3
            y <- x `divideBy` 7
            -- z <- return (y+1)
            let z = y+1
            z `divideBy` 3
        )

{-
instance Monad IO where
  return :: a -> IO a
  (>>=) :: IO a -> (a -> IO b) -> IO b
-}

-- getContents :: IO String
-- get string until (^D) from stdin

main0  :: IO ()
main0 = do
  str <- getContents
  _ <- putStrLn (combineshow 1 str)
  _ <- print test_tree
  _ <- print (fmap (\x->x+1) test_tree)
  let t2 = fmap (\x->x+1) test_tree
  print (fmap (\x -> show x ++ "!") t2)


main  :: IO ()
main =
  getContents >>= \str -> 
  putStrLn (combineshow 1 str) >>= \_ -> 
  print test_tree >>= \_ -> 
  print (fmap (\x->x+1) test_tree) >>= \_ -> 
  let t2 = fmap (\x->x+1) test_tree
  in print (fmap (\x -> show x ++ "!") t2)

{-
main  :: IO ()
main = getContents >>=
       (\str -> putStrLn (combineshow 1 str)) >>=
       (\_ -> print test_tree) >>=
       (\_ -> print (fmap (\x->x+1) test_tree)) >>=
       (\_ -> let t2 = fmap (\x->x+1) test_tree in print (fmap (\x -> show x ++ "!") t2))
-}
