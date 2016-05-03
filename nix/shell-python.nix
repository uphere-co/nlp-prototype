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


#(pkgs.python27.buildEnv.override rec {
#      extraLibs =  with pkgs.python27Packages; [ ipython feedparser numpy scipy pip
#      		   sqlite3 scikitlearn dateutil flask tornado toolz.Flask-Limiter toolz.nltk ];
      #shellHook = ''
      #    export PYTHONPATH=/nix/store/rc445bsxb245k2xb86y2w6qi1w6k8rpg-python-sqlite3-2.7.11/lib/python2.7/site-packages
      #'';
#      }).env



