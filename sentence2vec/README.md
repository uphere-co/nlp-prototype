#How to build
```
#One can make the build directory anywhere.
mkdir build
cd build
#RNNPP_PATH must be set.
cmake {path of sentence2vec} -DRNNPP_PATH=../rnn++
#DRNNPP_PATH must be a relative path, not an absolute path.
make -j8
```
