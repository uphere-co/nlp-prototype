import Control.Monad.Trans.Class (lift)
import System.Console.Haskeline

myaction :: InputT IO (Maybe String,Maybe String)
myaction = do
  x <- getInputLine "x> "
  lift (print x) -- :: InputT IO ()
  y <- getInputLine "y> "
  lift (print y) -- :: InputT IO ()
  return (x,y)

main :: IO ()
main = do
  putStrLn "haskeline example"
  ret <- runInputT defaultSettings myaction
  print ret
