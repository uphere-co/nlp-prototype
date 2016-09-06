#rm A_supl.o #clib2.o test.o
#g++ -c  -fPIC --std=c++14  A_supl.cc
#g++ --std=c++14 -c clib2.cxx
#ld -r clib.o clib2.o -o clibcomb.o

#ghc test.hs A_supl.o -lstdc++

g++ -c -fPIC --std=c++14 stub.cc 
ghc test.hs stub.o -lstdc++


#ghc -O2 -fforce-recomp -funbox-strict-fields test.hs clibcomb.o  -lstdc++
#string.o

