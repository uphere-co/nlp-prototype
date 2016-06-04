# NLP prototype

`python` has python codes and `cpp` has c++ codes obviously. Currently we have word2vec experiments.

## RecursiveNN directory
See tests for usages. Use pytest for unit testing.
```bash
#Run unit tests:
cd python/RecursiveNN
py.test
```
## word2vec python directory

### Parsing Arxiv meta XMLs 
First, launch `ipcluster start -n 12`.
Run `parse_arxiv_meta.ipynb` files in notebook, or in shell:
```bash
##Set environment variable ARXIV_META
export ARXIV_META='the location of arxiv meta files'
./run_in_script.sh
```

### Running word2vec server
```bash
export WORD2VEC_MODEL='path of gensim.Word2Vec model'
ipython word2vec_server.py
```
```bash
#list of servers
## localhost:10100 : words -> most similar words
input : words seperated by space, e.g. `quark` or `electron muon tau`.
output : similar words seperated by space.
## localhost:10101 : words -> 2D PCA figs
input : words seperated by space, e.g. `quark` or `electron muon tau`.
output : full path of the stored .png file.
```

### Running notebooks via SSH tunneling
Use SSH tunneling:
```bash
#On your local machine:
ssh -p 22221 -N -f -L localhost:18888:localhost:8888  216.218.134.74
#To stop tunneling, find the process and kill it:
netstat -ltnp | grep 18888
#from remote server:
cd nlp-prototype/
nix-shell nix/shell-python.nix
jupyter notebook
```
Then, visit localhost:18888 on your computer browser to run notebooks.

### Note on cuDNN
Currently, NixOS 16.03 does not support cuDNN. To run Theano with cuDNN, use following steps:
1. Make `~/.nixpkgs/config.nix` file:
```bash
{
  allowUnfree=true; #cudnnSupport=true;                                                                                                        
  theano.cudnnSupport=true;
}
```
2. checkout repo based on 15.09:
```bash
mkdir -p repos/srcc
cd repos/srcc
git clone https://github.com/wavewave/nixpkgs.git
cd nixpkgs
git checkout haskellcudafix-on-15.09-cudnntest

#Register cuDNN binary file:
nix-store --add-fixed sha256 /data/groups/uphere/cuda/cudnn-7.0-linux-x64-v4.0-prod.tgz
```
3. Run nix-shell:
```bash
nix-shell nix/shell-python.cudnn.nix --arg pkgs "import ${HOME}/repo/srcc/nixpkgs {}"
#check Theano is running with CUDA:
python -c 'from theano.sandbox.cuda.dnn import version; print(version())'
```

Note. 
For detail, check following source files:
```bash
cat ~/.nix-defexpr/channels_root/nixos/default.nix
cat ~/repo/srcc/nixpkgs/pkgs/top-level/python-packages.nix
cat ~/repo/srcc/nixpkgs/pkgs/development/python-modules/theano/default.nix
grep -B 10 -A 10 theano  ~/repo/srcc/nixpkgs/pkgs/top-level/python-packages.nix
```
