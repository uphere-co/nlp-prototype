import Data.Char
import Data.List.Split

filename = "parsed_ipg160105.txt"

main = do
  txt <- readFile filename

  let xs = lines txt
      splitter = split  . dropFinalBlank . keepDelimsR $ whenElt (\x -> take 13 x == "=============")
      ys = (map reformat . splitter) xs


  let (_,_,test) = head ys

  mapM_ print (findWords test)


toPair (x:y:_) = (x,y)
                       
reformat xs =
  let idxline = last xs
      (snum,idee) = toPair . splitOneOf ":=" . drop 14 $ idxline
  in (snum, idee, concat (init xs))

findWords = filter (not . null) . splitWhen (not . isAlphaNum) 

