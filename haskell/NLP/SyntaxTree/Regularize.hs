{-# LANGUAGE OverloadedStrings #-}

module NLP.SyntaxTree.Regularize where

import           Data.Char      (isDigit)
import qualified Data.List as L
import           Data.Text      (Text)
import qualified Data.Text as T (drop, filter, head, null, reverse, splitOn, take, toLower)
--
import NLP.SyntaxTree.Type
--
import Debug.Trace

isPunctuation :: Text -> Bool
isPunctuation x = x `elem` [ ".", "?", "!", ","
                           , "\"", "'", "`"
                           , "''" , "\"\"" , "``"
                           , "\"\" \"\"", "`` ``" , "'' ''"
                           , "'''"
                           ]

isxRB :: Text -> Bool
isxRB x = x' == "lrb" || x' == "rrb" where x' = T.toLower x


removeApostrophe txt
  | txt == "'s" = "his"  -- this is very tempororay.
  | txt == "'ll" = "will"
  | txt == "'m" = "am"
  | otherwise = txt
             
removeTrailingPunctuation = T.reverse . T.filter (not . (`elem` ['.',',',' '] )  ) . T.reverse 

removeDollarAnnot :: Text -> Text
removeDollarAnnot txt = if (T.take 2 txt == "$ ") then T.drop 2 txt else txt

replaceNumber :: Text -> Text
replaceNumber txt = if isDigit (T.head txt) then "some-number" else txt

expandComposite :: Text -> BinTree Text
expandComposite txt = let lst = T.splitOn "-" txt
                      in (L.foldr1 BinNode . map BinLeaf) lst 


isRemovable :: Text -> Bool
isRemovable x = T.null x || isPunctuation x || isxRB x

nullify :: BinTree Text -> BNTree Bool (Bool,Text)
nullify (BinLeaf x) = if isRemovable x then BNTLeaf (True,x) else BNTLeaf (False,x) 
nullify (BinNode x y) =
    let x' = nullify x
        y' = nullify y
    in if (isNulled x' && isNulled y') then BNTNode True x' y' else BNTNode False x' y'

isNulled :: BNTree Bool (Bool,Text) -> Bool
isNulled (BNTLeaf (b,x)) = b
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
        
        regtext = {- removeDotFromMrMrs .-} removeApostrophe . removeTrailingPunctuation .  removeDollarAnnot . T.toLower

