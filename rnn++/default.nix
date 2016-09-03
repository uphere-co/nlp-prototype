{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz     = callPackage ../nix/default-python.nix {
                  inherit pkgs;
                  buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
                };
    toolz_cpp = callPackage ../nix/default-cpp.nix { };
in
stdenv.mkDerivation rec {
  version = "0.0";
  name = "rnn++-${version}";
  src = ./.;
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
                 ]) 
                   ++ 
                 [
                   wget zip unzip
                   cmake pkgconfig clang_38 
                   boost
                   hdf5 hdf5-cpp liblbfgs 
                   tbb openblas  
                   toolz_cpp.msgsl
                   toolz_cpp.spdlog
                 ];
  #installPhase = ''
  #  mkdir -p $out/bin
  #  cp hdf5io model1 parser_similarity train_model0 train_model1 $out/bin 
  #'';
}
