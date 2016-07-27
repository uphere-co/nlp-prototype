{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ./default-python.nix {
              pkgs=pkgs;
              buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
            };
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
                   Theano Keras h5py
                   pytest toolz.pytest-mock
                   toolz.guppy
                   toolz.nltk toolz.bllipparser
                 ]) 
                   ++ 
                 [
                   wget jdk zip unzip
                   cmake pkgconfig clang_38 clang-analyzer
                   boost
                   hdf5 hdf5-cpp zeromq
                   tbb openblas  
                 ];
  shellHook = ''
     EDITOR=vim
     CC=clang
     CXX=clang++
  '';
}
