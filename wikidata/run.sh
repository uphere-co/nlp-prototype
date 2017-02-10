ghc -O2 -funbox-strict-fields  -fforce-recomp -threaded -rtsopts parser.hs
time ./parser +RTS -N20 -H10G -s -RTS
