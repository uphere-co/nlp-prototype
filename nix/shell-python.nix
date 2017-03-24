{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz     = callPackage ./default-python.nix {
                  inherit pkgs;
                  buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
                };
    toolz_cpp = callPackage ./default-cpp.nix { };
in
stdenv.mkDerivation {
  name = "python-env";
  buildInputs = (with python27Packages;
                 [ ipython jupyter ipyparallel
                   line_profiler
                   matplotlib seaborn
                   numpy scipy pandas scikitlearn
                   pyzmq
                   redis
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
                   toolz.feedparser beautifulsoup4
                 ]) 
                   ++ 
                 [
                   wget jdk zip unzip which stress htop
                   cmake pkgconfig clang clang-analyzer elfutils
                   gcc6
                   #libcxx libcxxabi
                   boost
                   hdf5 hdf5-cpp liblbfgs 
                   cppzmq zeromq
                   tbb openblas  
                   linuxPackages.perf
                   toolz_cpp.msgsl
                   toolz_cpp.spdlog
                   toolz_cpp.fmt
		   toolz_cpp.json
                   toolz_cpp.csv
                   toolz_cpp.backwardcpp
                   toolz_cpp.xxhashct
                   toolz_cpp.variant
                   toolz_cpp.flatbuffers
                   doxygen graphviz
                   libcgroup 
                   redis
                   libpqxx
                   ucspi-tcp
                   jq pigz
                 ];
  shellHook = ''
     PS1="\n\[\033[0;34m\][\u@\h.devel:\w]\$\[\033[0m\] "
     #PS1="\e[0;33[\u@\h.devel:\w]\$\e[m "
     EDITOR=vim
     CC=clang
     CXX=clang++
     MODEL=/data/groups/uphere/parsers/corenlp/
     CORENLP=/data/groups/uphere/parsers/corenlp/
     PARSER=/data/groups/uphere/parsers/corenlp/stanford-parser-full-2015-12-09/
     CLASSPATH=$CORENLP/stanford-corenlp.Oct2016.jar:$MODEL/stanford-english-corenlp-2016-01-10-models.jar:$CORENLP/stanford-ner.jar;
     #CLASSPATH=$CLASSPATH:$CORENLP/stanford-corenlp-3.6.0.jar:$PARSER/stanford-parser.jar:$CORENLP/slf4j-simple.jar:$CORENLP/slf4j-api.jar:$MODEL/stanford-english-corenlp-2016-01-10-models.jar:$MODEL/stanford-parser-english-2016-01-10-models.jar:$MODEL/stanford-srparser-2014-10-23-models.jar:$MODEL/
  '';
}
