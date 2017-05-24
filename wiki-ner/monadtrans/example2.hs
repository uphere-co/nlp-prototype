import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT(runStateT),modify)
import System.Console.Haskeline
import System.Console.Haskeline.MonadException

-- get put
-- modify

myaction :: InputT (StateT Int IO) () -- (Maybe String,Maybe String)
myaction = do
  x <- getInputLine "x> "
  lift (modify (+1))                                -- :: InputT (StateT Int IO) ()
  lift (lift (putStrLn ("x = " ++ show x)))              -- :: InputT (StateT Int IO) ()
  y <- getInputLine "y> "
  lift (modify (+1))                                -- :: InputT (StateT Int IO) ()
  lift (lift (putStrLn ("y = " ++ show y)))              -- :: InputT (StateT Int IO) ()
  -- liftIO (putStrLn ("y = " ++ show y))
  case (x,y) of
    (Nothing,_) -> return () 
    (_,Nothing) -> return ()
    _ -> myaction

  -- liftIO 
  -- return (x,y)

main :: IO ()
main = do
  putStrLn "haskeline example"
  ret <- runStateT (runInputT defaultSettings myaction) (0 :: Int)
  print ret
