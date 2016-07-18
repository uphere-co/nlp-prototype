# NLP prototype

`rnnparser` has python implementation of symbolic computaton module and recursive neural network parser.
`word2vec` has c++ codes. Currently we have word2vec experiments.
`paraphrase` and `symbolic` contains haskell projects on them.
## `rnnparser` 
See `README.md` inside it for details.

##paraphrase
### Run RNN parser result visualizer
```bash
nix-shell paraphrase/nix/shell.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}"
cabal sandbox init
cabal build
dist/build/rnnresult/rnnresult /data/groups/uphere/tmp/nlp-data/rnnparser-result/rnn.sample
```
