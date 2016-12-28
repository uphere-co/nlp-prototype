{ pkgs, haskellPackages }:

with pkgs;

let 
    #hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    #newHaskll
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