import           Data.Char
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.List.Split

filename = "parsed_ipg160105.txt"

fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

main = do
  txt <- readFile filename

  let ls = lines txt
      splitter = split  . dropFinalBlank . keepDelimsR $ whenElt (\x -> take 13 x == "=============")
      docs = (map reformat . splitter) ls


  mapM_ (\x -> (putStrLn . display) x >> putStrLn "------" ) docs -- (take 10 docs)

display x = let snum = fst3 x 
                docid = snd3 x
                wmap = (makeWordMap . trd3) x
            in snum ++ ":" ++ docid ++ ":" ++ show wmap

toPair (x:y:_) = (x,y)
                       
reformat xs =
  let idxline = last xs
      (snum,idee) = toPair . splitOneOf ":=" . drop 14 $ idxline
  in (snum, idee, concat (init xs))

findWords = filter (not . null) . splitWhen (not . isAlphaNum) 

add1Word m w = HM.alter f w m
  where f Nothing = Just 1
        f (Just n) = Just (n+1) 

makeWordMap = foldl' add1Word HM.empty . findWords 
