CXX = g++
CFLAGS = -march=native -fopenmp -funroll-loops -Wall -Wno-unused-result --std=c++11 # -pthread -Ofast -g
LDFLAGS = -lm -lpca -larmadillo -lgfortran -lopenblas #-pthread
BOOSTFLAGS = -lboost_iostreams -lboost_system -lboost_filesystem

#libopenblas.a  #-lblas -llapack -lpca #-larmadillo #`pkg-config opencv --libs`

all: bh_tsne word2vec distance pca grassmannian

utils.o: utils.cpp utils.h
	$(CXX) -c $(CFLAGS) utils.cpp

sptree.o: sptree.cpp sptree.h
	$(CXX) -c sptree.cpp -O2

WordEmbed.o: WordEmbed.cpp WordEmbed.h utils.h
	$(CXX) -c $(CFLAGS) WordEmbed.cpp -O2

bh_tsne: tsne.cpp utils.h sptree.o
	$(CXX) -o bh_tsne tsne.cpp sptree.o -O2


# Executables

word2vec: word2vec.cpp utils.h WordEmbed.h utils.o WordEmbed.o
	$(CXX) $(LDFLAGS) $(BOOSTFLAGS) -o word2vec word2vec.cpp utils.o WordEmbed.o $(CFLAGS) -Ofast -pthread

distance: distance.cpp utils.h utils.o
	$(CXX) $(LDFLAGS) $(BOOSTFLAGS) -o distance distance.cpp utils.o $(CFLAGS) -Ofast

pca: pca.cpp utils.h utils.o
	$(CXX) $(LDFLAGS) $(BOOSTFLAGS) -o pca pca.cpp utils.o $(CFLAGS)

grassmannian : grassmannian.cpp
	$(CXX) $(LDFLAGS) $(BOOSTFLAGS) -o grassmannian grassmannian.cpp $(CFLAGS) -Ofast

clean:
	rm -rf utils.o sptree.o WordEmbed.o bh_tsne word2vec distance pca grassmannian
