{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Control.DeepSeq (($!!))

import System.FilePath (FilePath,(</>))
import System.IO (withFile,hGetLine,hGetContents,IOMode(ReadMode,WriteMode))
--import Control.Exception (bracket)
--import Network.HTTP.Wget (wget)

import System.Process
{-
lst = [ "ipg160105.zip", "ipg160112.zip", "ipg160119.zip", "ipg160126.zip", "ipg160202.zip"
      , "ipg160209.zip", "ipg160216.zip", "ipg160223.zip", "ipg160301.zip", "ipg160308.zip"
      , "ipg160315.zip", "ipg160322.zip", "ipg160329.zip", "ipg160405.zip"
      ] 
-}

dataList = "./uspto_list.txt" :: FilePath
downloadPath = "/data/groups/uphere/uspto/" :: FilePath

-- Is there a way which is file descriptor leak safe
-- and doesn't need deepseq?
readDataList :: IO [(FilePath,String)]
readDataList =
    withFile dataList ReadMode $
    \h -> do
      cont <- hGetContents h
      let res = fmap splitIt $ lines cont
      return $!! res
  where
    splitIt s =
      case words s of
        [a,b] -> (a::FilePath,b)
        _ -> ("","")

md5sumCheck :: FilePath -> String -> IO Bool
md5sumCheck fn m = do
    res <- readProcess "md5sum" [fn] ""
    let m' = head $ words res
    return $ m == m'

fetchFile :: FilePath -> IO ()
fetchFile fn = do
    system ("wget https://bulkdata.uspto.gov/data2/patent/grant/redbook/fulltext/"
      ++ fn 
      ++ "-O " ++ (downloadPath </> fn))
    return ()

main = do
    xs <- readDataList
    forM_ xs doSomething
  where
    doSomething (fn,m) = do
        let fullFn = downloadPath </> fn
--        putStrLn $ show fullFn ++ " \n" ++ m
        q <- md5sumCheck fullFn m
        if q then
            putStrLn $ fn ++ " already exists."
        else do
            putStrLn $ fn ++ " needs to be downloaded."
            fetchFile fn

--  mapM_ (\x -> system ("wget https://bulkdata.uspto.gov/data2/patent/grant/redbook/fulltext/2016/" ++ x)) lst
--  mapM_ (\x -> system ("unzip " ++ x)) lst
