{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Foldable
import           Data.List.Split             (chunksOf)
import           Data.Text                   (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.IO
--
import           Debug.Trace

           
skipUntilPara :: A.Parser ()
skipUntilPara = do
    A.skipWhile (/= '<')
    (A.string "<P>" >> return ()) <|> (A.anyChar >> skipUntilPara)

sentence :: A.Parser Text
sentence = skipUntilPara >> A.skipSpace >> A.takeTill (== '<') <* A.string "</P>" 

main :: IO ()
main = do
    putStrLn "extracting sentenses."
    txt <- TIO.readFile "LDC2003T05"
    let r = A.parseOnly (many sentence) txt
    case r of
      Left err -> print err
      Right lst -> do
        print (length lst)
        let lsts = zip [1..] (chunksOf 5000 lst)
        forM_ lsts $ \(n,txts) -> 
          withFile ("sentences" ++ show n ++ ".txt") WriteMode $ \h ->
            forM_ txts $ \txt -> do
              TIO.hPutStrLn h "<s>"
              TIO.hPutStr   h txt
              TIO.hPutStrLn h "</s>"

        
