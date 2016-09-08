{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import           Data.Map       (Map)
import qualified Data.Map  as M
import           NLP.Types


main = do
  let 
      (e,s) =
        runIdentity . flip runStateT emptyTS . runEitherT $ do
            r1 <- binleafM (1 :: Int)
            r2 <- binleafM 2
            r3 <- binnodeM r1 r2
            binnodeM r1 r3
  let m = currentGraph s
  (print . M.keys) m

  case e of
    Left str -> error str
    Right r -> do
      case graph2tree m r of
        Left str' -> error str'
        Right r' -> putStrLn $ binprint r'
  -- let btree= binnode (binnode (binleaf 1) (binleaf 2)) (binleaf 3)
  -- putStrLn $ binprint btree  
