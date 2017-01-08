import Control.Applicative

{- 
f :: * -> * 

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  
class (Functor f) => Applicative f where
  pure :: a -> f a  ( == return)
  <*> :: f (a -> b) -> f a -> f b

class (Applicative m) => Monad m where
  return = pure
  (>>=) = flip bind
  bind :: (a -> m b) -> m a -> m b
  (>>=) :: m a -> (a -> m b) -> m b

  example
instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b


Alternative is like monoid

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

class (Applicative f) => Alternative f where
  empty :: f a    ( ~ mempty )    
  (<|>) :: f a -> f a -> f a   ( ~ mappend )
  -- some :: f a -> f [a]
  -- many :: f a -> f [a]
  
instance Alternative Maybe where
  empty = Nothing
  Nothing <|> Nothing = Nothing
  (Just x) <|> Nothing = Just x
  Nothing <|> (Just x) = Just x
  (Just x) <|> (Just y) = Just x
-}

main = do
  let m1 :: Maybe Int
      m1 = Just 3
      m2 :: Maybe Int
      m2 = Nothing
      m3 :: Maybe Int
      m3 = Just 4 
      -- m = m1 >>= \x -> m2 >>= \y -> m3 >>= \z -> return (x,y,z)
      r1 :: Maybe (Int,Int,Int)
      r1  = do { x <- m1;
                 y <- m2;
                 z <- m3;
                 return (x,y,z) }  -- && 
      r2 :: Maybe Int    
      r2 = m1 <|> m2 <|> m3        -- || 
      r3 :: Maybe (Int,Int,Int)
      r3  = do { x <- (m1 <|> return 0);
                 y <- (m2 <|> return 0);
                 z <- (m3 <|> return 0);
                 return (x,y,z) }
      
  print r1
  print r2
  print r3
  print (many m1)
