./word2vec -train text8 -output vec.txt -save-vocab data.vocab
./pca -word-vector vec.txt
