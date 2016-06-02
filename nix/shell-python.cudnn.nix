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
                 [ ipython
                   #ipyparallel
                   numpy scipy
                   #jupyter
                   pyzmq
                   theano h5py pip
                   toolz.gensim toolz.KerasCUDNN
                 ]) ++ [ wget screen gfortran ];
  shellHook = ''
  '';
}