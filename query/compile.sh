g++ -std=c++14 -c invoke.cpp

ghc test.hs invoke.o -lsimilarity  -lparser -ltbb -lhdf5_cpp -lstdc++
