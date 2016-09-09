import           Data.Char
import           Data.Foldable
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.List.Split
import           System.IO
import           Text.Printf

filename = "parsed_ipg160105.txt"

fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x


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

add1WordInWordDocMap docid m w = HM.alter f w m
  where f Nothing  = Just 1 -- [docid]
        f (Just n) = Just (n+1)  -- (Just lst) = Just (docid:lst)

appendWordDocMap m docid ws = foldl' (add1WordInWordDocMap docid) m ws

process1Doc m doc@(snum,docid,txt) = do
  let wmap = makeWordMap txt
      -- summary = snum ++ ":" ++ docid ++ ":" ++ show wmap
      ks = HM.keys wmap
  hPutStrLn stderr (show (snum,docid))
  return $! appendWordDocMap m docid ks


showMap m = let lst = HM.toList m
                lst' = sortBy (flip compare `on` snd) lst
                f (k,v) = printf "%20s : %d" k v
            in mapM_ (putStrLn . f) lst'
               
main = do
  txt <- readFile filename

  let ls = lines txt
      splitter = split  . dropFinalBlank . keepDelimsR $ whenElt (\x -> take 13 x == "=============")
      docs = (map reformat . splitter) ls

  -- mapM_ (\x -> (putStrLn . display) x >> putStrLn "------" ) docs -- (take 10 docs)
  rs <- foldlM process1Doc (HM.empty :: HM.HashMap String Int) docs -- (take 100 docs )
      -- rs' = zip [1..] rs
  -- mapM_ print (take 10000 rs' )

  showMap rs
