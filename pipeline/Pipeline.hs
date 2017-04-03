{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--
import           Type
import           Run
import           System.Directory       (createDirectoryIfMissing)
import           Options.Applicative

data PipelineOption = PipelineOption { _minorVersion :: Int
                                     , _output       :: String
                                     }

pOptions :: Parser PipelineOption
pOptions = PipelineOption <$> option auto (long "minor version" <> short 'v' <> help "Minor version")
           <*> strOption (long "output path" <> short 'o' <> help "Output path")

pipelineOption = info pOptions ( fullDesc <> progDesc "Pipeline App" <> header "options are minor-version, output path")

main :: IO ()
main = do
  opt <- execParser pipelineOption    
  
  writeJSON YGP
  writeJSON RSS
  mkRnnApp
  mkCoreNLPApp
  doWikidataAnalysis
  runYGP
  runRSS
  calculateWI
