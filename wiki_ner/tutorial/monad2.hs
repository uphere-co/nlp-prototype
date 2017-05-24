{-# LANGUAGE FlexibleInstances #-}

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b


-- class (Functor m) => Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- m :: * -> *

{- 
f :: Int -> a 

newtype F a = MkF (Int -> a)

newtype T = MkT Int 

Tree :: * -> *

Maybe :: * -> *
-}

--- (r ->) *

{-
(->) r :: * -> *
(->) :: * -> * -> * 
-}

{-
instance  Functor ((->) r) where
  -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
  -- pointful style (point = variable)
  -- fmap f mx i = f (mx i)
  -- pointless style
  fmap f mx = f . mx

-- const :: a -> b -> a
-- const x y = x
-}

{-
instance Monad ((->) r) where
  -- return :: a -> (r -> a)
  -- return x = const x
  return = const
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: (r -> a) -> (a -> r -> b) -> r -> b 
  -- f :: r -> a
  -- conn :: a -> r -> b
  -- (f >>= conn) i :: b
  ((>>=) f conn) i = conn (f i) i  

   --  :: b
-}


-- Applicative
-- <$> = fmap 
-- <*> = ap

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b


-- class Functor f => Applicative f where

-- class (Applicative m) => Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- Functor => Applicative => Monad
-- Functor => Monad
-- Ap


{-
class (Functor f) => Applicative f where
  pure :: a -> f a
  ap :: f (a -> b) -> f a -> f b

-- pure :: (a -> b) -> f ( a -> b)


example
f = std::vector

f Int 
std::vector<Int>

f (Int -> Int)
std::vector<std::function<int,int>*>

{-
 a   a -> b  b -> c

a => a
b => b -> c
ap :: f (a -> b) -> f a -> f b
ap :: f (a -> (b -> c)) -> f a -> f (b -> c)
ap :: f (a -> b -> c) -> f a -> f (b -> c)
ap :: f (a -> b -> c -> d ) -> f a -> f (b -> c -> d)
ap fabcd fa = fbcd
ap fbcd fb = fcd
ap fcd fc = fd

((fabcd `ap` fa) `ap` fb) `ap` fc = fd
fabcd `ap` fa `ap` fb `ap` fc = fd
ap = <*>
fabcd <*> fa <*> fb <*> fc 
-}

now example!

instance Applicative IO 

fab :: IO (a -> b) 

ma :: IO a

getContents :: IO String

getFileSize  :: IO Int
fab <*> ma
fab :: IO (String -> String)
fabc :: IO (String -> String -> String)
fabc <*> getContents <*> getContents

fabc :: IO (String -> Int -> String)
fabc <*> getContents <*> getFileSize

tell :: String -> Int -> String
tell str n = str ++ " : " ++ show n 

pure fabc <*> getContents <*> getFileSize
-------------
fmap fabc getContents <*> getFileSize 

fmap = <$>
fabc <$> getContents <*> getFileSize 

f :: a -> b
fa :: f a

pure f = f (a -> b)

op = \f fa -> pure f <*> fa

op :: (a -> b) -> f a -> f b 
op f fa = pure f <*> fa

op = fmap
-}

--    fbcd `ap` fb = fcd
  {-
main = do
  let m1 ::                              Int -> a
      m2 :: a -> Int -> String         ~ Int -> a -> String 
      m3 :: String -> Int -> something ~ Int -> String -> something
      
    
      m :: Int -> String
      m = do a <- m1
             b <- m2 a
             return b
-}

ask :: r -> r
ask = id


main' = do
  let  shout :: (Show a) => a -> String -> String
       shout x msg = msg ++ " : " ++ show x
       m = do let f x = x + 1
                  y = f 3
              msg' <- shout y
              msg2 <- shout 8 
              return (msg',msg2) 

  print (m "mymesg")
  print (m "new message format")

         

  
{- Reader monad. Environment ..   Configuration -}
    
--(r ->) :: * -> *
-- == (->) r

{- 
instance Applicative ((->) r) where
  pure = const
  -- ap :: f (a -> b) -> f a -> f b
  -- ap :: (r -> a -> b) -> (r -> a) -> (r -> b)
  -- f :: r -> a -> b
  -- ma :: r -> a
  -- x :: r
  -- f x :: a -> b
  -- ma x :: a
  ap f ma x :: b
  ap f ma x = (f x) (ma x) :: b

  --
  fabc `ap` fa `ap` fb :: r -> c
  (fabc `ap` fa `ap` fb) x
  = ((fabc `ap` fa) `ap` fb) x    
  = ((fabc `ap` fa) x) (fb x)
     -----------------
  = ((fabc x) (fa x)) (fb x)
  = (fabc x) (fa x) (fb x)


  (fabcdef <*> fa <*> fb <*> ....) x
      = (fabcdef x) (fa x) (fb x) ...
    
  f x y = (f x) y
-}
--   fabcdef = pure abcdef
--   abcdef <$> fa <*> fb <*> fc.... 

data Test = Test { testa :: Int
                 , testb :: String
                 , testc :: Double }

{-
testa :: Test -> Int
testb :: Test -> String
testc :: Test -> Double
-}

xs = [Test 3233 "ewfo" 1.32, Test 2020 "abc" 2.32]

main = do
  print $ map ((,,) <$> testa <*> testb <*> testc) xs
  print $ map ((+) <$> testa <*> (floor . testc)) xs  

