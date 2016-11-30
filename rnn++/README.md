# C++ implementation of RecursiveNN
## Build
```
cmake {path of nlp-prototype/rnn++}
make -j8
./app
```
## Usages
- app/train_model1.cpp : train RNN model1. 
 1. Set params in `config.h` and `config.cpp` accordingly.
 - `./app`
- app/parser_model1.cpp : parse sentences with a given RNN params
 1. set HDF5 store file `rnn_param_store_name` in `config.cpp`
 - `./model1 {model1.UUID.IthMiniBatch} 1b.testset > parsed_file_name`
- app/parser_model1.cpp : measure similarity between two parsed files
 1. ./parser_similarity parsed_file1 parsed_file2

# Doxygen 
## Usages
Just run `doxygen` inside this directory, then documentation is generated and stored in HTML_OUTPUT of `Doxyfile` file, `/data/groups/uphere/doxygen/rnn++/`, presently.

# Query Engine
## Usages
```
# Getting/indexing YGP db data
./etl_ygpdb config.ygp.json -1

# query engine test
echo Batteries mercury restriction > query.0
./nndep_reader config.ygptest.json query.0
```

