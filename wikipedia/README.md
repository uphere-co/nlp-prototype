# Haskell script for running bash commends parallely
## Usage
`parsing.hs` : Run Stanford parser and BLLIP parser:
```
#Prepare input files; split the large file.
split -d -l 100000 1b.training.short_sentences.known 1b.training.short_sentences.known.
#Edit&run script!
ghc parsing.hs
./parsing
```

#Read/processing Penn Treebank dataset
## Usage 
`ptb_reader.py` : extract raw text and Chomsky normalized binary tree from Penn treebank dataset
`filter_short_sents.py` : filter short sentences only from a pair of INFILE and INFILE.tree.
```
nix-shell shell-wiki.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}"
#treebank_3/parsed/mrg/wsj/test : directory containing WSJ sections
#wsj.test : output file name. 
#output : 
# - wsj.test and wsj.test.tree : contain all nodes
# - wsj.test.trim and wsj.test.trim.tree : nodes with '-NONE-' tag is removed.
ipython ptb_reader.py ~/treebank_3/parsed/mrg/wsj/test wsj.test
#One can specify a file, if needed:
# - input: ~/word2vec/news/news.2014.en.shuffled.ptb.stanford 
# - output : out.trim and out.trim.tree
ipython ptb_reader.py  ~/word2vec/news/ out news.2014.en.shuffled.ptb.stanford
```
