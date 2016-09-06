rm clib.o clib2.o test.o
g++ --std=c++14 -c clib.cxx
g++ --std=c++14 -c clib2.cxx
ghc -O2 -fforce-recomp -funbox-strict-fields test.hs  clib2.o clib.o  -lstdc++
#string.o

