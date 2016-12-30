{ pkgs, haskellPackages }:

with pkgs;

let 
    hsenv = haskellPackages.ghcWithPackages (p: with p; [ fficxx-runtime fficxx ]);
in

stdenv.mkDerivation {
  name = "query-binding-src";
  buildInputs = [ hsenv ];
  src = ./.; 
  buildPhase = ''
    ghc QueryGen.hs
    ./QueryGen
  '';
  installPhase = ''
    mkdir -p $out
    cp -a query-binding/* $out
  '';

}