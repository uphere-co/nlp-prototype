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
                   matplotlib seaborn
                   numpy scipy pandas scikitlearn
                   pyzmq
                   cython
                   numba
                   toolz.gensim toolz.untangle
                   Theano Keras h5py
                   pytest toolz.pytest-mock
                   toolz.guppy
                   toolz.nltk
                 ]) ++ [cudnn gcc49 wget];
  shellHook = ''
     EDITOR=vim
  '';
}
