{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let toolz = callPackage ../nix/default-python.nix {
              pkgs=pkgs;
              buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
            };
in

let 
    hsenv = haskellPackages.ghcWithPackages (p: with p; [
              attoparsec orc
            ]);

in stdenv.mkDerivation {
     name = "jihuni-shell";
     buildInputs =  (with python27Packages;
                 [ ipython
                   toolz.nltk toolz.bllipparser
                 ]) ++  [ hsenv jdk ] ;
     shellHook = ''
        EDITOR=vim
     '';
   }
