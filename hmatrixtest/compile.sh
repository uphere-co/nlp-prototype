gcc -c -O3 -march=native -std=c++14 -I../rnn++ ../rnn++/utils/string.cpp
gcc -c -O3 -march=native -std=c++14 -I../rnn++ main.cpp
ghc -O2 -fforce-recomp -funbox-strict-fields test.hs -larmadillo main.o string.o

