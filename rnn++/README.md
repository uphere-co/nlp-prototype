# Source layout
## Important constraints for managing dependencies
Namespaces have their hiearachy :
1. base
2. low
3. high
4. application

One shall not include same or higher-level namespaces.

## List of namespaces
- `test`: application
  - In `tests/` :  for unit-testing
  - `tests/data` : dataset for unit-testing
- 'util` : base
  - In `utils/`    : contain general helper functions; i.e. not NLP specific.
- `wordrep` : low
  - In `wordrep/`  : data types for NLP applications; e.g. word embedding, named entity, ...
- `word2vec` : high
  - In `word2vec/` : for word2vec training
- `wiki` : high
  - In `wiki/`     : for Wikidata entity tagging
- `data` : high
  - In `data_source/` : contains ETL logics for YGP DB, RSS, and so on.
- `engine` : application
  - In `similarity/` : contains query engine implementation.

Other directories : 
- `app/`      : contains executables
- `rnn/`      : for recursive neural networks(RNN)
- `parser/`   : for parser based on RNN
- `models/`   : contains RNN models.
- `wrapper/`  : for query-daemon
- `scripts/`  : contain some bash and python scripts.


# C++ implementation of RecursiveNN
## Build
```bash
# rnn++ does not compile with a latest master branch of nixpkgs. I check that it compiles with the commit 8a424e3fbd (Nov 2016) of wavewave's fork of nixpkgs.
cmake {path of nlp-prototype/rnn++}
make -j20
./app
```
## Usages
- app/train_model1.cpp : train RNN model1. 
  1. Set params in `config.h` and `config.cpp` accordingly.
  2. `./app`
- app/parser_model1.cpp : parse sentences with a given RNN params
  1. set HDF5 store file `rnn_param_store_name` in `config.cpp`
  2. `./model1 {model1.UUID.IthMiniBatch} 1b.testset > parsed_file_name`
- app/parser_model1.cpp : measure similarity between two parsed files
  1. ./parser_similarity parsed_file1 parsed_file2

# Doxygen
## Usages
Just run `doxygen` inside this directory, then documentation is generated and stored in HTML_OUTPUT of `Doxyfile` file, `/data/groups/uphere/doxygen/rnn++/`, presently.

# Query Engine
## Usages
```bash
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
```bash
#Get unigram distribution of YGP DB:
./ygpdb_dump ~/word2vec/ygp/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines | ./word_count | ./word_count_collect words.h5
#Get lists of words
./ygpdb_dump ~/word2vec/ygp/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines | ./word_count | cut -d' ' -f1
```
## Getting word importance scores:
The executable `word_importance_build` takes no input parameters. 
Instead, it has hard coded parameters to specify which column is for "summary" of which column;
it refers the file specified in "column_uids_dump" field of config files for that.
Output filename is also fixed : `prob.h5`. Generate it and place it appreciatively.
```bash
#First, place config.rss.json and config.ygp.json to present directory.
./word_importance_build
```

## Building word similarity table :
- Input: a config JSON to get
  1. Word importance
  2. Word2vec embedding
- Output : a single binary file (`wordsim.table.bin` for below example)
```bash
# Example :
./wordsim_table config.rss.json.1 wordsim.table.bin
```

## Indexing YGP DB:
```
#Dumping YGP DB:
#Usage : ./ygpdb_dump COLUMNS_TO_DUMP DUMP_DIR > aa
#COLUMNS_TO_DUMP : a same format used in the file mentioned in a "column_uids_dump" field of config JSON file.  
#DUMP_DIR : store dumped text files in there. Do not generate dump files if it is empty.
#Example usages : generate dump files for indexing
./ygpdb_dump ~/word2vec/ygp/column.uid ~/word2vec/ygp/country_col.uid /opt/YGP.text > ygp.text 2>ygp.text.log
#Dump to stdout only for word2vec training:
./ygpdb_dump ~/word2vec/ygp/column.uid | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > ygp.text.ptb

#collect words
cat ygp.text.ptb | ./word_count | awk '{print $1}' >> all_words.duplicate
# Deduplicate words :
cat all_words.duplicate | ./word_count | awk '{print $1}' >> all_words
#Check `config.ygp.json` uses the upodated `all_words` file.

#Collect JSON dumps of YGP DB row elements. 
#Beware the `-name` option to not include a directory itself.
find ~/word2vec/ygp.corenlp/  -name '*.*.*.*' > ygp.corenlp
# Run indexer with config JSON file. It generates file specified in "dep_parsed_store" field of the config file.
# CAUSTION : set MINOR_VERSION larger than existing one; othervise, it emits error "File already exists."
# MINOR_VERSION : integer

make -j20 && time ./ygpdb_etl config.ygp.json ygp.corenlp MINOR_VERSION
#Test with phrases_in_sentence to see if everything works well
make -j20 && ./nndep_reader config.ygptest.json query.0

#For testing dataset, randomly select N elements:
sort -R ygp.corenlp | head -n 10000 > aaa
```

## Indexing RSS data
### Adapt updated indexing scheme
- cp ygp/all_words rss/
  - Note that this file, specified in "word_uids_dump" field of config, should be updated for a new dataset.
  - The update is not essential for query engine, but necessary for human(e.g. for testing and debugging).
  - See "Incremental word2vec training" section for getting new "unseen" words. 
- cp ygp/news.h5 rss/
- cp ygp/prob.test.h5 rss/
- Update word_uids_dump field of RSSQueryEngine config JSON.

### Build a dataset
- Prepare config JSON for a dataset
  1. It needs word vector embedding. 
  2. Either use eixsting one or build custum word embedding from it(See "Incremental word2vec training" section for details).
- Index the dataset with them
```
# Parse HTML dumps to extract texts:
# Get list of hashes for each table (i.e. source like "NYT", or others)
find /opt/NYT.dump/ -type f -printf "%f\n" > NYT.hashes
# Note that as more dumps are added to the dataset, index will be changed because ordering of the new files and old files are mixed up.
find /opt/NYT.dump/ -type f | cat -n | xargs -P 20 -i'{}' python ../rss_crawler/parse_article.py NYT {} /opt/RSS.text/
# Depenency parsing of the texts
find /opt/RSS.text -type f | xargs -P20 -I {} python ../rnn++/scripts/corenlp.py {} /opt/RSS.json/
# Indexing to build HDF5 store file. The "1" is a minor version of the indexed dataset. 
find /opt/RSS.json/ -type f > rss_jsons

# Generate indexed texts from CoreNLP JSON output dumps
./rss_dump config.rss.json rss_jsons rss.parsed
# Move generated binary files
mv rss.parsed.* /opt/testset.rss/blocks/
# Generate Wikidata annotation files
./wikidata_annotator config.rss.json 
```

## Run an app as a network daemon
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

1. Get new words in a new dataset.
2. Select words whose occurrence is larger than 9
3. Derive word embedding for the new set of words from an existing word embedding
   * For known words : reuse their word embedding 
   * For unseen words : a weighted sum of its context words
4. Build unigram distribution of the new dataset
5. Evaluate a quality of word vector embedding

```
# extract words from a voca of a config (only for debugging; not necessary for other process)
./list_words ~/word2vec/rss/news.h5 news.en.uids ~/word2vec/rss/all_words > voca

# Get words in a dataset
## YGP case:
find /opt/YGP.text/ -type f | xargs awk '{print}' | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > ygp.text.ptb
cat ygp.text.ptb | ./word_count | awk '$2>9{print}' > ygp.text.ptb.counts
cat ygp.text.ptb.counts | awk '{print $1}' > ygp.text.ptb.words
## RSS case:
find /opt/RSS.text/ -type f | xargs awk '{print}' | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > rss.text.ptb
cat rss.text.ptb | ./word_count | awk '$2>9{print}' > rss.text.ptb.counts
cat rss.text.ptb.counts | awk '{print $1}' > rss.text.ptb.words

# Pipe new words in a new dataset to get their word embedding and store it to new HDF5 store "test.h5"
cat rss.text.ptb.words | ./word_context config.rss.json test.h5
# Build unigram distribution for the new dataset
cat rss.text.ptb.counts | ./word_count_collect config.rsstest.json unigram.h5
# Evaluate quality of word embedding.
./word2vec_eval config.rssw2v.json unigram.h5
```

## Wikidata entity annotation
```
# Preparing data
# Get wikidata.nes from corenlp/wiki/wikidata_ner
cat wikidata.nes | awk -F '\t' '$2=="True"{print $1}' > wikidata.uid.ne
cat wikidata.items | awk -F '\t' 'NF==5{print $1}' > wikidata.uid
# On the `mark` server, takes ~12min
lbzcat /scratch/groups/uphere/wikidata/wikidata-20170904-all.json.bz2 | ./wikidata_etl | grep -v "^P" > wikidata.items
cat wikidata.items | awk -F '\t' '{print $1 "\t" $NF}' > wikidata.all_entities
# IMPORTANT: Currently, only the P31 properties are used. It should be extended as the inference logic evolves
cat wikidata.items | awk -F '\t' 'NF==5{print $1 "\t" $3}' > wikidata.properties
# Test annotation
./wikidata_annotator config.rss.json QUERY_FILE
```

## Build tests
```
nix-build nix/release.nix -A query --argstr nixpkgs $HOME/repo/srcc/nixpkgs --max-jobs 20 --cores 20
```
