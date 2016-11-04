module QueryServer.Type where

import qualified Data.Binary as Bi
import           Data.Hashable
import           Data.Text         (Text)

data Query = Query { querySentences :: [ Text ] } -- deriving Typeable
  deriving (Show,Eq,Ord)

instance Bi.Binary Query where
  put (Query xs) = Bi.put xs
  get = Query <$> Bi.get

instance Hashable Query where
  hashWithSalt n (Query txts) = hashWithSalt n txts

data HeartBeat = HB { heartBeat :: Int }

instance Bi.Binary HeartBeat where
  put (HB n) = Bi.put n
  get = HB <$> Bi.get
  
  



   
