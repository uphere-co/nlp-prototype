rm clib.o clib2.o test.o
g++ --std=c++14 -c clib.cxx
g++ --std=c++14 -c clib2.cxx
#ld -r clib.o clib2.o -o clibcomb.o

ghc -c AModule.hs
ld -r AModule.o clib.o clib2.o -o AModule_comb.o 
ghc test.hs AModule_comb.o -lstdc++ 
#ghc -O2 -fforce-recomp -funbox-strict-fields test.hs clibcomb.o  -lstdc++
#string.o

