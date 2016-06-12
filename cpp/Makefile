CXX = g++
CFLAGS = -Ofast -Wall -funroll-loops -Wno-unused-result
LDFLAGS = -lcblas

all: word2vec

word2vec: word2vec.cpp
	$(CXX) -o word2vec word2vec.cpp $(CFLAGS) $(LDFLAGS) --std=c++11

clean:
	rm -rf word2vec
