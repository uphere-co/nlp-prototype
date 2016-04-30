{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.SyntaxTree.Printer where

import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text as T  (concat,intercalate,replicate,take)
--
import NLP.SyntaxTree.Types

textprinter :: Int -> PennTree -> Text
textprinter n (PT _ lst) = T.intercalate "\n" (map (textprinter (n+4)) lst)
textprinter n (PN txt) = T.replicate n " " <> txt

treeprinter :: Int -> PennTree -> Text
treeprinter n (PT t lst) = "\n" <> fmttag <> T.concat (map (treeprinter (n+2)) lst)
  where fmttag = T.replicate n " " <> T.take 4 (t <> "    ") <> " "
treeprinter n (PN txt) = txt

btreeprinter :: [Bool] -> BinTree Text -> Text
btreeprinter bs (BinLeaf txt) = txt
btreeprinter bs (BinNode a b) = "\x252C\x2500" <> btreeprinter (bs++[True]) a <> "\n" <>
                                drawlines bs <> "\x2514\x2500" <> btreeprinter (bs++[False]) b
  where
    drawlines bs = foldMap (\case True -> "\x2502 " ; False -> "  ") bs 


bntPrint :: [Bool] -> (e -> Text) -> (a -> Text) ->  BNTree e a -> Text
bntPrint bs _     lshow (BNTLeaf l)     = lshow l
bntPrint bs nshow lshow (BNTNode e a b) =
    "\x2299" <> nshow e <> "\n" <>
    drawlines bs <> "\x251C\x2500" <> bntPrint (bs++[True]) nshow lshow a <> "\n" <>
    drawlines bs <> "\x2514\x2500" <> bntPrint (bs++[False]) nshow lshow b
  where
    drawlines bs = foldMap (\case True -> "\x2502 " ; False -> "  ") bs

-- utility functions

convert :: BinTree Text -> BNTree Text Text
convert (BinLeaf a) = BNTLeaf a
convert (BinNode a b) = BNTNode "node" (convert a) (convert b)
