{-# LANGUAGE OverloadedStrings #-}

module NLP.SyntaxTree.Regularize where

import           Data.Char      (isDigit)
import qualified Data.List as L
import           Data.Text      (Text)
import qualified Data.Text as T (drop, find, filter, head, null, reverse, splitOn,
                                 tail, take, toLower)
--
import NLP.SyntaxTree.Type

isPunctuation :: Text -> Bool
isPunctuation x = x `elem` [ ".", "?", "!", ",", ":", ";"
                           , "\"", "'", "`"
                           , "''" , "\"\"" , "``"
                           -- , "\"\" \"\"", "`` ``" , "'' ''"
                           -- , "'''"
                           ]

isxRB :: Text -> Bool
isxRB x = x' == "lrb" || x' == "rrb" where x' = T.toLower x

expandShortened :: Text -> Text
expandShortened txt
  | txt == "'s"  = "his"  -- this is very tempororay.
  | txt == "'ll" = "will"
  | txt == "'m"  = "am"
  | txt == "n't" = "not"
  | txt == "'re" = "are"
  | txt == "'ve" = "have"
  | txt == ".."  = "ellipsis"
  | txt == "..." = "ellipsis"
  | otherwise = txt
    

removeTrailingPunctuation :: Text -> Text
removeTrailingPunctuation = T.reverse . T.filter (not . (`elem` ['.',',',' '] )  ) . T.reverse 

removeDollarAnnot :: Text -> Text
removeDollarAnnot txt = if (T.take 2 txt == "$ ") then T.drop 2 txt else txt

replaceNumber :: Text -> Text
replaceNumber txt | T.null txt = ""
replaceNumber txt | otherwise  = 
    let (h,t) = (T.head txt, T.tail txt)
        r | isDigit h                      = "some-number"
          | h == '.' && T.null t           = "."
          | h == '.' && isDigit (T.head t) = "some-fractional-number"
          | h == '\'' && T.null t           = "'"
          | h == '\'' && isDigit (T.head t) = "some-year"    -- year  
          | otherwise                      = txt
    in r 

expandComposite :: Text -> BinTree Text
expandComposite txt = 
    let lst = T.splitOn "-" txt
    in if length lst > 1
         then case T.find isDigit txt of
                Nothing -> (L.foldr1 BinNode . map BinLeaf) lst
                Just _  ->  BinLeaf txt
         else BinLeaf txt


isRemovable :: Text -> Bool
isRemovable x = T.null x || isPunctuation x || isxRB x

nullify :: BinTree Text -> BNTree Bool (Bool,Text)
nullify (BinLeaf x) = if isRemovable x then BNTLeaf (True,x) else BNTLeaf (False,x) 
nullify (BinNode x y) =
    let x' = nullify x
        y' = nullify y
    in if (isNulled x' && isNulled y') then BNTNode True x' y' else BNTNode False x' y'

isNulled :: BNTree Bool (Bool,Text) -> Bool
isNulled (BNTLeaf (b,_)) = b
isNulled (BNTNode b _ _) = b

regularize :: BinTree Text -> BinTree Text
regularize = fmap regtext . remove . nullify . regcomp . fmap replaceNumber
  where 
        remove (BNTNode False x y)
          | isNulled x = remove y
          | isNulled y = remove x
          | otherwise  = BinNode (remove x) (remove y)
        remove (BNTLeaf (False,x)) = BinLeaf x
        remove x = error (show x)

        regcomp (BinLeaf x) = expandComposite x
        regcomp (BinNode x y) = BinNode (regcomp x) (regcomp y)
        
        regtext = removeTrailingPunctuation . expandShortened . removeDollarAnnot . T.toLower

