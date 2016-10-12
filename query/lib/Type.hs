module Type where

import qualified Data.Binary as Bi


data Query = Query { querySentences :: [ String ] } -- deriving Typeable
  deriving Show
           
instance Bi.Binary Query where
  put (Query xs) = Bi.put xs
  get = Query <$> Bi.get

data HeartBeat = HB { heartBeat :: Int }

instance Bi.Binary HeartBeat where
  put (HB n) = Bi.put n
  get = HB <$> Bi.get
  
  



   
