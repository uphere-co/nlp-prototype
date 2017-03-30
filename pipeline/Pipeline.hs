{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--
import           Type
import           Run

main :: IO ()
main = do
  writeJSON YGP
  writeJSON RSS
  mkRnnApp
  mkCoreNLPApp
  doWikidataAnalysis
  runYGP
  runRSS
  calculateWI
