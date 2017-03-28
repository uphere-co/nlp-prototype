{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--
import           Type
import           Run

main :: IO ()
main = do
  mkRnnApp
  mkCoreNLPApp
  runYGP
  runRSS
