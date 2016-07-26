{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    hsenv = haskellPackages.ghcWithPackages (p: with p; [
              attoparsec
            ]);

in stdenv.mkDerivation {
     name = "jihuni-shell";
     buildInputs = [ hsenv ];
     shellHook = ''
     '';
   }
