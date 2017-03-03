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
#Get lists of words
./ygpdb_dump ~/word2vec/ygp/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines | ./word_count | cut -d' ' -f1

```

## Indexing YGP DB:
```
#Run word counter with std::string token type
./ygpdb_dump ~/word2vec/ygp/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > ygp.raw
#collect words
cat ygp.raw | ./word_count | cut -d' ' -f1 >> all_words
#Check `config.ygp.json` uses the upodated `all_words` file.

#Collect JSON dumps of YGP DB row elements. 
#Beware the `-name` option to not include a directory itself.
find ~/word2vec/ygp.corenlp/  -name '*.*.*.*' > ygp.corenlp
#Run indexer with config JSON file.
make -j20 && time ./ygpdb_etl config.ygp.json ygp.corenlp
#Test with phrases_in_sentence to see if everything works well
make -j20 && ./nndep_reader config.ygptest.json query.0

#For testing dataset, randomly select N elements:
sort -R ygp.corenlp | head -n 10000 > aaa
```
Launch word counter as a TCP server :
```
```
## Indexing RSS data
### Adapt updated indexing scheme
- cp ygp/all_words rss/
- cp ygp/news.h5 rss/
- cp ygp/prob.test.h5 rss/
- Update word_uids_dump field of RSSQueryEngine config JSON.


### Build a hash list and a list of corenlp dump files
```
#Find all dump files  | show sha256 hash only | word count on them | select hashes with count 3 only(i.e. have all columns)
find ~/word2vec/NYT.corenlp/ -name '*.*.*'  -printf "%f\n" | cut -d'.' -f1 | ./word_count | awk -F" " '$2==3{print $1}' > article.hashes
find ~/word2vec/NYT.corenlp/ -name '*.*.*'  -printf "%f\n" | cut -d'.' -f1 | ./word_count | awk -F" " '{print $1}' > article.hashes
find ~/word2vec/NYT.corenlp/ -name '*.*.*' > article.corenlp
```

## Build test dataset

- Prepare config JSON for test dataset.
- Build voca vecs by pruning unnecessary words for test dataset
- Index the test dataset with it
- Test and use it!

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

## Run stress test of query engine:
```
# Get short sentences from YGP db
./ygpdb_dump c | awk 'NF>2&&NF<10{print }' | head -n 1000 > queries.ygp.short
# Get short sentences from news summaries
find ~/word2vec/NYT.text/ -name '*.summary' | xargs awk '{print }' | awk 'NF>2&&NF<20{print }' | head -n 1000 > queries.rss.short
# Run stress test:
# Prepare a directory for output
mkdir answers
# Run it.
./stress_query_engine config.rss.json queries.rss.short >a 2>b
./stress_query_engine config.ygp.json queries.ygp.short >a 2>b

# Shows query results of stress test
ls answers/*output | python ../rnn++/tests/query_engine_acceptance.py
```

## Incremental word2vec training
### List words in existing voca
```
./show_words_in_voca config.ygp.json > known_words
#List newly seen words in a dataset
## YGP case:
./ygpdb_dump ~/word2vec/ygp/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > ygp.text
cat ygp.text | ./word_count > ygp.word_count
cat ygp.word_count | ./word_count_collect config.ygp.json > ygp.new_words
## RSS case:
find ~/word2vec/NYT.text/ -name '*.*' -not -path '/home/jihuni/word2vec/NYT.text/' | xargs awk '{print }' | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > rss.text
cat rss.text | ./word_count > rss.word_count
cat rss.word_count | ./word_count_collect config.rss.json > rss.new_words
```
### Getting count of context words 
```
cat ygp.new_words | cut -d' ' -f1 | ./word_context config.ygp.json > b
```

### Parsing large number of HTML dump of news articless
```
#Split files into chunks
ls ~/word2vec/article | split -d -a 3 -l 10000 - articles.
```

## Wikidata entity annotation
```
# Preparing data
# Get wikidata.nes from corenlp/wiki/wikidata_ner
cat wikidata.nes | awk -F '\t' '$2=="True"{print $1}' > wikidata.uid.ne
cat wikidata.items | awk -F '\t' 'NF==5{print $1}' > wikidata.uid
cat ~/word2vec/wikidata-20170206-all.json | ./wikidata_etl >wikidata.items
cat wikidata.items | awk -F '\t' '{print $1 "\t" $NF}' > wikidata.all_entities
# IMPORTANT: Currently, only the P31 properties are used. It should be extended as the inference logic evolves
cat wikidata.items | awk -F '\t' 'NF==5{print $1 "\t" $3}' > wikidata.properties
cat wikidata.all_entities | ./wikidata_annotator config.rss.json
```

## Build tests
```
nix-build nix/release.nix -A query --argstr nixpkgs $HOME/repo/srcc/nixpkgs --max-jobs 20 --cores 20
```
