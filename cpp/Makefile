CXX = g++
CFLAGS = -g -Ofast -Wall -funroll-loops -Wno-unused-result --std=c++11
LDFLAGS = -larmadillo -lpca #-lblas -llapack -lpca #-larmadillo #`pkg-config opencv --libs`

all: word2vec pca

word2vec: word2vec.cpp
	$(CXX) $(LDFLAGS) -o word2vec word2vec.cpp $(CFLAGS)

pca: pca.cpp
	$(CXX) $(LDFLAGS) -o pca pca.cpp $(CFLAGS)

clean:
	rm -rf word2vec pca
