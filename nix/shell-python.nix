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
                   toolz.gensim toolz.untangle
                   toolz.Theano toolz.Keras h5py
                   pytest_28 toolz.pytest-mock
                 ]);
  shellHook = ''
  '';
}
