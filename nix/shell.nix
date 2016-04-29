{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let hsenv = haskellPackages.ghcWithPackages (p: with p; [
              xml-conduit split unordered-containers data-binary-ieee754
	      spool
            ]);
in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv ];
     shellHook = ''
     '';
   }