{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ./default-python.nix {
              pkgs=pkgs;
              buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
            };
in
stdenv.mkDerivation {
  name = "python-env";
  buildInputs = with python27Packages;
                 [ ipython feedparser numpy scipy pip
                   sqlite3 scikitlearn dateutil flask
                   tornado toolz.Flask-Limiter toolz.nltk  ];
  shellHook = ''
  '';
}
