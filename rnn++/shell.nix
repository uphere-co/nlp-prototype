{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay? <uphere-nix-overlay>
}:

with pkgs;

let toolz     = callPackage (uphere-nix-overlay + "/nix/default-python.nix") {
                  inherit pkgs;
                  buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
                };
    toolz_cpp = callPackage (uphere-nix-overlay + "/nix/default-cpp.nix") { };
    config = import ./config.nix { inherit pkgs toolz_cpp; };
    mystdenv = clangStdenv;
in
mystdenv.mkDerivation {
  name = "python-env";
  buildInputs =  [
                   wget jdk zip unzip which stress htop
                   cmake pkgconfig clang-analyzer
                   linuxPackages.perf
                   zeromq
                   doxygen graphviz
                   libcgroup 
                   lbzip2
                 ] ++ config;
  shellHook = ''
     PS1="\n\[\033[0;34m\][\u@\h.devel:\w]\$\[\033[0m\] "
     EDITOR=vim
     CC=clang
     CXX=clang++
     MODEL=/data/groups/uphere/parsers/corenlp
     CORENLP=/data/groups/uphere/parsers/corenlp/stanford-corenlp-full-2015-12-09
     PARSER=/data/groups/uphere/parsers/corenlp/stanford-parser-full-2015-12-09
     CLASSPATH=$CLASSPATH:$CORENLP/stanford-corenlp-3.6.0.jar:$PARSER/stanford-parser.jar:$CORENLP/slf4j-simple.jar:$CORENLP/slf4j-api.jar:$MODEL/stanford-english-corenlp-2016-01-10-models.jar:$MODEL/stanford-parser-english-2016-01-10-models.jar:$MODEL/stanford-srparser-2014-10-23-models.jar:$MODEL/
  '';
}
