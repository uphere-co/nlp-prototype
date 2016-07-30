# NLP prototype

`rnnparser` has python implementation of symbolic computaton module and recursive neural network parser.
`word2vec` has c++ codes. Currently we have word2vec experiments.
`paraphrase` and `symbolic` contains haskell projects on them.
## `rnnparser` 
See `README.md` inside it for details.

##paraphrase
### Run RNN parser result visualizer
```bash
#It is recommended to use `haskellcudafix-on-15.09` branch of `nixpkgs`:
cd $HOME/repo/srcc/nixpkgs
git fetch origin
git checkout -b haskellcudafix-on-15.09 origin/haskellcudafix-on-15.09
cd $NLP_PROTOTYPE
nix-shell paraphrase/nix/shell.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}"
#Remove cabal cache if required:
rm -rf ~/.cabal/
cd paraphrase/autoencode
cabal sandbox add-source ../../symbolic
cabal install --dependencies-only
cabal build
dist/build/rnnresult/rnnresult /data/groups/uphere/tmp/nlp-data/rnnparser-result/rnn.sample
```
##wikipedia
This contains scripts for data preprocessing, especially for large data files such as WikiPedia.
- `xml_splitter.py` : XML stream processing to split large XML files.
- `process.hs` : Processes bash commands in parallel
- `parser.py` : Uses NLTK for using various parsers and to get output for binary tree viewer `rnnresult` in `paraphrase` module.

##ygp_data_script
- load_ygp_db.py : Python PostgreSQL driver to load YGP db tables. To run it, use `nix/shell-python.nix` configuration. Using it in `ipython` shell by `%run load_ygp_db.py.py`; it will be useful for data exploration.
