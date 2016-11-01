{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz     = callPackage ../nix/default-python.nix {
                  inherit pkgs;
                  buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
                };
    toolz_cpp = callPackage ../nix/default-cpp.nix { };
    config = import ./config.nix { inherit pkgs toolz_cpp; };
in
stdenv.mkDerivation {
  name = "python-env";
  buildInputs = (with python27Packages;
                 [ ipython jupyter ipyparallel
                   line_profiler
                   matplotlib seaborn
                   numpy scipy pandas scikitlearn
                   pyzmq
                   cython
                   numba
                   toolz.gensim toolz.untangle
                   #Theano Keras
		   h5py
                   pytest toolz.pytest-mock
                   toolz.guppy
                   toolz.nltk toolz.bllipparser
                   psycopg2
                   #cgroup-utils
                   toolz.cldoc
                 ]) 
                   ++ 
                 [
                   wget jdk zip unzip which stress htop
                   cmake pkgconfig clang_38 clang-analyzer
                   linuxPackages_4_6.perf
                   zeromq
                   doxygen graphviz
                   libcgroup 
                 ] ++ config;
  shellHook = ''
     EDITOR=vim
     CC=clang
     CXX=clang++
     MODEL=/data/groups/uphere/parsers/corenlp
     CORENLP=/data/groups/uphere/parsers/corenlp/stanford-corenlp-full-2015-12-09
     PARSER=/data/groups/uphere/parsers/corenlp/stanford-parser-full-2015-12-09
     CLASSPATH=$CLASSPATH:$CORENLP/stanford-corenlp-3.6.0.jar:$PARSER/stanford-parser.jar:$CORENLP/slf4j-simple.jar:$CORENLP/slf4j-api.jar:$MODEL/stanford-english-corenlp-2016-01-10-models.jar:$MODEL/stanford-parser-english-2016-01-10-models.jar:$MODEL/stanford-srparser-2014-10-23-models.jar:$MODEL/
  '';
}
