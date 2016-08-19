{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let toolz = callPackage ../nix/default-python.nix {
              pkgs=pkgs;
              buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
            };
in

let 
    hsenv = haskellPackages.ghcWithPackages (p: with p; [
              attoparsec orc split
            ]);

in stdenv.mkDerivation {
     name = "jihuni-shell";
     buildInputs =  (with python27Packages;
                 [ ipython
                   toolz.nltk toolz.bllipparser
                 ]) ++  [ hsenv jdk ] ;
     shellHook = ''
         EDITOR=vim
         CC=clang
         CXX=clang++
         MODEL=/data/groups/uphere/corenlp
         CORENLP=/data/groups/uphere/corenlp/stanford-corenlp-full-2015-12-09
         PARSER=/data/groups/uphere/corenlp/stanford-parser-full-2015-12-09
         CLASSPATH=$CLASSPATH:$CORENLP/stanford-corenlp-3.6.0.jar:$PARSER/stanford-parser.jar:$CORENLP/slf4j-simple.jar:$CORENLP/slf4j-api.jar:$MODEL/stanford-english-corenlp-2016-01-10-models.jar:$MODEL/stanford-parser-english-2016-01-10-models.jar:$MODEL/

     '';
   }
