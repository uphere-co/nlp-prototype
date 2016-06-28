CXX = g++
CFLAGS = -g -march=native -pthread -fopenmp -funroll-loops -Wno-unused-result -Ofast --std=c++11 # -Wall -Ofast
LDFLAGS = -pthread -lpca -larmadillo -lgfortran -lopenblas
BOOSTFLAGS = -lboost_iostreams -lboost_system -lboost_filesystem

#/home/wavewave/repo/srcp/nlp-prototype/tmp/OpenBLAS-0.2.17/libopenblas.a



#/nix/store/nn5fmpqghzq6dx6g441py15f1p9a1q4h-openblas-0.2.17/lib/libopenblas.a

#libopenblas.a  #-lblas -llapack -lpca #-larmadillo #`pkg-config opencv --libs`

all: pca word2vec

word2vec: word2vec.cpp
	$(CXX) $(LDFLAGS) $(BOOSTFLAGS) -o word2vec word2vec.cpp $(CFLAGS)

pca: pca.o
	$(CXX) -o pca pca.o $(LDFLAGS) $(BOOSTFLAGS) 

pca.o: pca.cpp
	$(CXX) -c $(CFLAGS) pca.cpp 
clean:
	rm -rf word2vec pca pca.o
