import Control.Monad.ST
import Data.STRef

main = do

  let x = runST $ do
            sref <- newSTRef (0 :: Int)
            modifySTRef sref (+1)
            readSTRef sref
            return sref
  print x
