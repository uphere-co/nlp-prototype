ghc -O2 -funbox-strict-fields  -fforce-recomp -threaded -rtsopts parser.hs

./parser slave localhost 9901  &
./parser slave localhost 9902 & 
./parser slave localhost 9903 & 
./parser slave localhost 9904 & 
./parser slave localhost 9905 & 
./parser slave localhost 9906 & 
./parser slave localhost 9907 & 
./parser slave localhost 9908 & 
./parser slave localhost 9909 & 
./parser slave localhost 9910 & 
./parser slave localhost 9911  &
./parser slave localhost 9912 & 
./parser slave localhost 9913 & 
./parser slave localhost 9914 & 
./parser slave localhost 9915 & 
./parser slave localhost 9916 & 
./parser slave localhost 9917 & 
./parser slave localhost 9918 & 
./parser slave localhost 9919 & 
./parser slave localhost 9920 & 


time ./parser master localhost 9900 +RTS -N1 -H10G -s -RTS
