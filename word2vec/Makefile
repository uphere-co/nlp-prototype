CXX = g++
CFLAGS = -g -march=native -fopenmp -funroll-loops -Wall -Wno-unused-result --std=c++11 # -pthread -Ofast
LDFLAGS = -lpca -larmadillo -lgfortran -lopenblas #-pthread
BOOSTFLAGS = -lboost_iostreams -lboost_system -lboost_filesystem

#libopenblas.a  #-lblas -llapack -lpca #-larmadillo #`pkg-config opencv --libs`

all: bh_tsne word2vec distance pca

utils.o: utils.cpp
	$(CXX) -c $(CFLAGS) utils.cpp

sptree.o: sptree.cpp
	$(CXX) -c sptree.cpp -O2

bh_tsne: tsne.cpp sptree.o
	$(CXX) -o bh_tsne tsne.cpp sptree.o -O2

word2vec: word2vec.cpp utils.o
	$(CXX) $(LDFLAGS) $(BOOSTFLAGS) -o word2vec word2vec.cpp utils.o $(CFLAGS) -Ofast -pthread

distance: distance.cpp utils.o
	$(CXX) $(LDFLAGS) $(BOOSTFLAGS) -o distance distance.cpp utils.o $(CFLAGS) -Ofast -pthread

pca: pca.cpp utils.o
	$(CXX) $(LDFLAGS) $(BOOSTFLAGS) -o pca pca.cpp utils.o $(CFLAGS)  

clean:
	rm -rf utils.o sptree.o bh_tsne word2vec distance pca
