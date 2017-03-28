mkdir build
cd build
ghc -o pipeline ../pipeline.hs ../Type.hs
rm ../pipeline.hi
rm ../pipeline.o
rm ../Type.hi
rm ../Type.o
