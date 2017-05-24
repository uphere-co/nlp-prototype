
test :: Maybe Int
test = do
  x <- Just 3
  y <- Nothing -- Just 4
  z <- Just 4
  return (x+z)

test2 :: Maybe Int
test2 = Just 3 >>= \x -> Nothing >>= \y -> Just 4 >>= \z -> return (x+z)

operation :: IO (Maybe Int)
operation = do
  x <- readFile file
  
  case x of
    Nothing -> return Nothing
    Just str -> do
      y <- readFile file2
      case y of
        Nothing -> return Nothing
        Just str' -> do
          return (Just (length (str++str')))

newtype (MaybeT m) a = MaybeT { runMaybeT :: m (Maybe a) }


{-  
  T x y = (T x) y
   :: *
   
  :: * -> *
  
  m (Maybe a) 

(MaybeT m) = Monad -}

-- Functor
-- Applicative
-- Monad

instance (Monad m) => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return v = MaybeT { runMaybeT = return (Just v) }
{-   a -> Maybe a
  (Maybe a) -> m (Maybe a)
  return
  (>>=) -}

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT mma >>= f = 
     MaybeT (mma >>= \ma ->
              case ma of
                Nothing -> return Nothing
                Just a -> runMaybeT (f a) ) 

safeReadFile :: FilePath -> IO (Maybe String)
MaybeT (safeReadFile fp) :: MaybeT IO String

mreadFile = MaybeT . safeReadFile

operation' :: MaybeT IO Int
operation' = do
  (x :: String) <- mreadFile file :: MaybeT IO String --  MaybeT (readFile file) 
  y <- mreadFile file2 -- MaybeT (readFile file2)
  
  z <- MaybeT (readFile file3 :: IO String >>= \z' -> return (Just z'))
  return (length (x ++ y))


action :: IO a

maction :: MaybeT IO a
maction = MaybeT (action >>= \z -> return (Just z))

lift :: (Monad m) => m a -> MaybeT m a 
lift action = MaybeT (action >>= \z -> return (Just z))

class MonadTrans t where
  lift :: (Monad m) => m a -> (t m) a

instance MonadTrans MaybeT where
  lift action = MaybeT (action >>= \z -> return (Just z))

mtry :: (IO a) -> IO (Maybe a)

mttry :: IO a -> MaybeT IO a 
mttry = MaybeT . mtry 

m a 
get
put
modify
Application = StateT Int IO   
-- readFile :: FilePath -> IO String 


main :: IO ()
main = do
  result :: Maybe Int <- runMaybeT operation' -- try
  case result of
    Nothing -> error   -- catch 
    Just n -> print n

{- 
:: STM ()

atomically :: STM () -> IO ()

main :: IO ()
main = do
  atomically $ do
    x <- takeTVar ref1
    y <- takeTVar ref2
    putTVar ref1 (x + 1)
    putTVar ref2 (y - 1)
-}

-- lock

--  TypeT m 
  
-- m (Maybe a)

-- MaybeT 

-- ExceptionT
-- ErrorT
-- EitherT 
-- EitherT a  = Either a

-- StateT Int IO ()
-- ReaderT
-- LoggerT IO 
-- CoroutineT IO  -- suspend / resume 

  -- uphere ghcjs
 
  (business logic ) |     <==> Event handler   <=>
    1 2 3                 (web component javascript)

  CoroutineT (CoroutineT IO)

 lift   (1 step)

    
 liftIO  (base -> topmost)
    lift


transformers : monad transformer
mtl   : typeclass  


 StateT IO a
  get
  put

 MaybeT (StateT IO) a

  lift get
  lift put

EitherT String (MaybeT (StateT IO)) a
  lift (lift get)
  ...


  
 class MonadState m s  

instance MonadState (Etiher.... ) s
  get
  put


  get


type  LogProcess = ReaderT LogLock Process

 MonadProcess
 
spawnLocal :: Process a
kill :: Process

{-
   ma >>= \a -> mb >>= \b -> ... 
  Lazy IO
  unsafeInterleaveIO
 ma >>= f = ma -> a `seq` f a 
-}


main = do
  print test2
  
