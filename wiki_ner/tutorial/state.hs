{-# LANGUAGE TypeSynonymInstances #-}

-- Miranda -- lazy language (<-> side effect)
--  lazy = pure
-- Haskell : committee language of research for lazy evaluation
--
-- IO = side effect
-- String -> String (batch processing)
-- Continuation
-- Monad

-- World -> World

-- State -> State   -- side effect = stateful operation
-- state monad : monad that can manipulate a state..
{-
stateful operation:
(s,a) -> (s,b)

      o          o'
  a   ->    b    ->  c
 __        __      -----
  s         s'      s''


IO as a special case of Stateful operation
(World,a) -> (World,b)

            o
m a -> ( a -> m b ) -> m b
                       
m b -> ( b -> m c ) -> m c
                       
m c -> ( c -> m d ) -> m d

m should contain s.
       o
(a,s) -> (b,s)
~
a -> s -> (b,s)
~
a -> (s -> (b,s))
      === m b
-}

-- the following is not valid but conceptual.
{- 
type StateInt a = (Int -> (a,Int))

instance Functor StateInt where
  (fmap f sa) x = let (x',s') = sa x :: (a,Int)
                  in (f x',s')  :: (b,Int)              
    -- = sb x
    -- 
    x :: Int
    sa :: Int-> (a,Int)
    f :: a -> b
    sb :: Int -> (b,Int)


    f a -> f b 

     a  ->  b

-- f (x,y)   f (x) (y)



-- this is old def (before Applicative-Monad-Proposal (AMP))

instance Monad StateInt where
  return :: a -> StateInt a
         :: a -> (Int -> (a,Int))
         :: a -> Int -> (a,Int)

         -- (a,Int) -> (a,Int)
  return x s = (x,s)

  (>>=) :: StateInt a       -> (a -> StateInt b)       -> StateInt b
           (Int -> (a,Int)) -> (a -> (Int -> (b,Int))) -> (Int -> (b,Int))
               sa                       mab                     
  (sa >>= mab) s = let (a,s') = sa s 
                   in mab a s'


-- this is new def

instance Applicative StateInt where
  pure :: a -> StateInt a
       :: a -> (Int -> (a,Int))
       :: a -> Int -> (a,Int)

         -- (a,Int) -> (a,Int)
  pure x s = (x,s)

  (<*>) :: StateInt (a -> b)     -> StateInt a       -> StateInt b
        :: (Int -> ((a->b),Int)) -> (Int -> (a,Int)) -> (Int -> (b,Int))

  (sab <*> sa) s = let (f,s') = sab s
                       (a,s'') = sa s'
                   in (f a,s'')

  ((fabcde <*> fa) <*> fb) <*> fc <*> fd 
       fbcde
             fcde
            IO String  IO String   IO
  (,,) <$> getLine <*> getLine <*> getLine
  (st,st,st)

instance Monad StateInt where
  -- return = pure
  (>>=) = as defined above.



get :: State s s
    :: s -> (s,s)
get s = (s,s)

put :: s -> State s ()
    :: s -> (s -> ((),s))
put ns _ = ((),ns) 

-}

import Control.Monad.State 

-- newtype State s a = State { runState :: s -> (a,s) }
-- evalState
-- execState


-- sequence :: [m a] -> m [a]

main = do 
  let x = repeat 3  -- [3,3,3,3,3,3,3,,....]
      y = [1..]     -- [1,2,3,4,.. ]
  print $ take 3 x -- = [3,3,3]
  print $ take 4 x -- = [3,3,3,3]
  print $ take 3 y -- = [1,2,3]
  print $ take 4 y -- = [1,2,3,4]


  let s1 :: State Int Int
      s1 = do -- put 3
              x <- get
              put (x+5)
              y <- get
              return y

      s' = do z <- s1
              w <- s1
              return (z+w)
      s'' = (,) <$> s1 <*> s'
      s = sequence [s1, s1,s1,s1,s1]
      

  putStrLn "=============="
  print (runState s 0)
  putStrLn "--------------"
  print (execState s 0)
  print (evalState s 0)


-- State (STring) ( Datatyep)
