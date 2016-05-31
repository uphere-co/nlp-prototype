# NLP prototype

`python` has python codes and `cpp` has c++ codes obviously. Currently we have word2vec experiments.

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
#from remote server:
cd nlp-prototype/
nix-shell nix/shell-python.nix
jupyter notebook
```
Then, visit localhost:18888 on your computer browser to run notebooks.
