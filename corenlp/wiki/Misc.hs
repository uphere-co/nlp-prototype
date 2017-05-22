module Misc where

data IRange = IRange { beg :: Int
                     , end :: Int}
                deriving(Eq, Show)