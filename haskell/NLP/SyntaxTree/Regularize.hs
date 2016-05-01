{-# LANGUAGE OverloadedStrings #-}

module NLP.SyntaxTree.Regularize where

import           Data.Char      (isDigit)
import qualified Data.List as L
import           Data.Text      (Text)
import qualified Data.Text as T (drop, head, splitOn, take, toLower)
--
import NLP.SyntaxTree.Type

isPunctuation :: Text -> Bool
isPunctuation x = (T.head x `elem` [ '.','\"', ',', '\'', '`', '?', '!' ] )

removeDollarAnnot :: Text -> Text
removeDollarAnnot txt = if (T.take 2 txt == "$ ") then T.drop 2 txt else txt

replaceNumber :: Text -> Text
replaceNumber txt = if isDigit (T.head txt) then "some-number" else txt

expandComposite :: Text -> BinTree Text
expandComposite txt = let lst = T.splitOn "-" txt
                      in (L.foldr1 BinNode . map BinLeaf) lst 


regularize :: BinTree Text -> BinTree Text
regularize (BinLeaf x) = (expandComposite . replaceNumber . removeDollarAnnot . T.toLower) x
regularize (BinNode y@(BinLeaf a) z) =
    if isPunctuation a then regularize z else BinNode (regularize y) (regularize z)
regularize (BinNode y z@(BinLeaf a))
    = if isPunctuation a then regularize y else BinNode (regularize y) (regularize z)
regularize (BinNode y z) = BinNode (regularize y) (regularize z)
