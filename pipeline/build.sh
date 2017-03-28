mkdir build
cd build
ghc -o Pipeline ../Pipeline.hs ../Type.hs ../Run.hs
rm ../Pipeline.hi
rm ../Pipeline.o
rm ../Type.hi
rm ../Type.o
rm ../Run.hi
rm ../Run.o
