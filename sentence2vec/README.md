# Usages
## `similarity`
- find similar word/phrase vectors for a given query.
- It can be run as a server app of ZeroMQ`s request/reply pattern. Change `while(0)` to `while(1)` to use it.
```
./similarity PATH_TO_confi.json_ PATH_TO_query.json
```
# How to build
```
#One can make the build directory anywhere.
mkdir build
cd build
#RNNPP_PATH must be set.
cmake {path of sentence2vec} -DRNNPP_PATH=../rnn++
#DRNNPP_PATH must be a relative path, not an absolute path.
make -j8
```
