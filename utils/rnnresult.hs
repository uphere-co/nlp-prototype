{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Environment
--
import           NLP.SyntaxTree.Parser
import           NLP.SyntaxTree.Printer

main :: IO ()
main = do
  putStrLn "test"
  args <- getArgs
  let filename = args !! 0
  txt <- TIO.readFile filename
  let r = A.parseOnly (A.many1 (bintree <* A.skipSpace)) txt
  case r of
    Left err -> error err
    Right rs -> mapM_ (\tr -> TIO.putStrLn (btreePrint [] (T.pack . show) tr)) rs 

