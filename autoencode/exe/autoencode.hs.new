{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import           Data.Map       (Map)
import qualified Data.Map  as M
import           NLP.Types


main0 = do
  let 
      (e,s) =
        runIdentity . flip runStateT emptyTS . runEitherT $ do
            r1 <- binleafM (1 :: Int)
            r2 <- binleafM 2
            r3 <- binnodeM r1 r2
            binnodeM r1 r3
            
  let m = currentGraph s
  (print . M.keys) m

  r <- runEitherT $ do
    r <- hoistEither e
    r' <- hoistEither (graph2tree m r)
    liftIO $ putStrLn (binprint r')

  -- error handling
  case r of
    Left err -> putStrLn err
    Right _ -> return ()


main = do
  let 
      (e,s) =
        runIdentity . flip runStateT emptyTS . runEitherT $ do
            r1 <- bntleafM (1 :: Int)
            r2 <- bntleafM 2
            r3 <- bntnodeM 3 r1 r2
            r4 <- bntnodeM 4 r3 r1
            bntnodeM 5 r1 r4
            
  let m = currentGraph s
  (print . M.keys) m

  r <- runEitherT $ do
    r <- hoistEither e
    r' <- hoistEither (graph2tree m r)
    liftIO $ putStrLn (bntprint r')

  -- error handling
  case r of
    Left err -> putStrLn err
    Right _ -> return ()
