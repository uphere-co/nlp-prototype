# Haskell library on ETL tasks for CoreNLP
## Run tests in nix-shell
```
nix-shell shell.nix -I nixpkgs=~/repo/srcc/nixpkgs
cd tests
ghc -Wall corenlp_json.hs
./corenlp_json
```
