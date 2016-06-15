import System.Process

lst = [ "ipg160105.zip", "ipg160112.zip", "ipg160119.zip", "ipg160126.zip", "ipg160202.zip"
      , "ipg160209.zip", "ipg160216.zip", "ipg160223.zip", "ipg160301.zip", "ipg160308.zip"
      , "ipg160315.zip", "ipg160322.zip", "ipg160329.zip", "ipg160405.zip"
      ] 


main = do
  mapM_ (\x -> system ("wget https://bulkdata.uspto.gov/data2/patent/grant/redbook/fulltext/2016/" ++ x)) lst
  mapM_ (\x -> system ("unzip " ++ x)) lst
