{-# LANGUAGE OverloadedStrings #-}

module NLP.SyntaxTree.Regularize where

import           Data.Char      (isDigit)
import qualified Data.List as L
import           Data.Text      (Text)
import qualified Data.Text as T (drop, head, null, splitOn, take, toLower)
--
import NLP.SyntaxTree.Type
--
import Debug.Trace

isPunctuation :: Text -> Bool
isPunctuation x = x `elem` [ ".", "?", "!", ","
                           , "\"", "'", "`"
                           , "''" , "\"\"" , "``"
                           , "\"\" \"\"", "`` ``" , "'' ''"
                           ]

isxRB :: Text -> Bool
isxRB x = x' == "lrb" || x' == "rrb" where x' = T.toLower x

removeDotFromMrMrs "mr." = "mr"
removeDotFromMrMrs "mrs." = "mrs"
removeDotFromMrMrs x = x 

removeDollarAnnot :: Text -> Text
removeDollarAnnot txt = if (T.take 2 txt == "$ ") then T.drop 2 txt else txt

replaceNumber :: Text -> Text
replaceNumber txt = if isDigit (T.head txt) then "some-number" else txt

expandComposite :: Text -> BinTree Text
expandComposite txt = let lst = T.splitOn "-" txt
                      in (L.foldr1 BinNode . map BinLeaf) lst 

{- 
data NullableBinTree a = Null
                       | NBTNode (NullableBinTree a) (NullableBinTree a)
                       | NBTLeaf a
-}

isRemovable :: Text -> Bool
isRemovable x = T.null x || isPunctuation x || isxRB x
-- isRemovable (BinNode x y) = isRemovable x && isRemovable y

nullify :: BinTree Text -> BNTree Bool (Bool,Text)
nullify (BinLeaf x) = if isRemovable x then BNTLeaf (True,x) else BNTLeaf (False,x) 
nullify (BinNode x y) =
    let x' = nullify x
        y' = nullify y
    in if (isNulled x' && isNulled y') then BNTNode True x' y' else BNTNode False x' y'

-- nullifyOrRegularize (BinNode Null Null) = Null

isNulled :: BNTree Bool (Bool,Text) -> Bool
isNulled (BNTLeaf (b,x)) = b
isNulled (BNTNode b _ _) = b

regularize :: BinTree Text -> BinTree Text
regularize tr = trace (show tr') $ (fmap regtext . remove . nullify . regcomp . fmap replaceNumber) tr
  where tr' = (nullify . regcomp) tr
        remove (BNTNode False x y)
          | isNulled x = remove y
          | isNulled y = remove x
          | otherwise  = BinNode (remove x) (remove y)
        remove (BNTLeaf (False,x)) = BinLeaf x
        remove x = error (show x)

        regcomp (BinLeaf x) = expandComposite x
        regcomp (BinNode x y) = BinNode (regcomp x) (regcomp y)
        
        regtext = removeDotFromMrMrs . removeDollarAnnot . T.toLower



{- 
(BinLeaf x) =  x
regularize (BinNode y z)
  | isRemovable y = regularize z
  | isRemovable z = regularize y
  | otherwise     = BinNode (regularize y) (regularize z)
-}
-- regularize (BinNode y z)
--     = if isRemovable z then regularize y else BinNode (regularize y) (regularize z)
-- regularize (BinNode y z) = BinNode (regularize y) (regularize z)

