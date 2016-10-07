module Type where

import qualified Data.Binary as Bi


data Query = Query { querySentences :: [ String ] } -- deriving Typeable
  deriving Show
           
instance Bi.Binary Query where
  put (Query xs) = Bi.put xs
  get = Query <$> Bi.get

  



   
