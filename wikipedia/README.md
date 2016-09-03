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
```
nix-shell shell-wiki.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}"
#treebank_3/parsed/mrg/wsj/test : directory containing WSJ sections
#wsj.test : output file name. 
#output : wsj.test and wsj.test.tree
ipython ptb_reader.py ~/treebank_3/parsed/mrg/wsj/test wsj.test
```
