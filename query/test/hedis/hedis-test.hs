{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class  (liftIO)
import Database.Redis

main = do
  let cinfo = defaultConnectInfo
              { connectHost = "localhost"
              , connectPort = PortNumber 6379 }
                
  conn <- connect cinfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    hello <- get "hello"
    world <- get "world"
    liftIO $ print (hello,world)

 
