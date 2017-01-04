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
# ETL pipeline for query engines
## Relevant apps for word vector training.
Note that all apps use stdin and stdout for data I/O.
- `app/word_count`
 - word counting. 
 - takes stdin and outputs to stdout. The `tcpserver` of `ucspi-tcp` package can be used to deploy it as a service.
 - input : output of CoreNLP PTBTokenizer
- `app/ygpdb_dump`
 - dump YGP db to stdout.
 - input : a file with a list of columns to export; e.g. "column_uids_dump" file in JSON config of YGP query engine/
- `app/word_count_collect`
 - write sum of word count outputs of word_count into HDF5 file.
 - will be used as unigram distribution for word2vec trainer
 - input : ouput HDF5 filename

Example usages
```
#Get unigram distribution of YGP DB:
./ygpdb_dump ~/word2vec/ygp/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines | ./word_count | ./word_count_collect words.h5
```

Launch word counter as a TCP server :
```
#Standalone :
tcpserver mark 22224 ./index_words config.nyt.json
#With CoreNLP word tokenizer :
tcpserver mark 22224 sh -c "java edu.stanford.nlp.process.PTBTokenizer -preserveLines | ./index_words config.nyt.json"
```
Example client usage :
```
time cat news.2014.train  | nc mark 22224 > a
```


## Build tests
```
nix-build nix/release.nix -A query --argstr nixpkgs $HOME/repo/srcc/nixpkgs --max-jobs 20 --cores 20
```
