g++ -std=c++14 -c invoke.cpp && \
ghc query.hs invoke.o -lsimilarity  -lparser -ltbb -lhdf5_cpp -lstdc++ && \
ghc client.hs