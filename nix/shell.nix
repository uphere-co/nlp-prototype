{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let hsenv = haskellPackages.ghcWithPackages (p: with p; [
              xml-conduit split unordered-containers vector-algorithms storable-tuple
	      tagged either
              accelerate
              accelerate-io
	      accelerate-cuda
	      mersenne-random
	      math-functions
            ]);
in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv cudatoolkit ];
     shellHook = ''
       export CUDA_PATH=${pkgs.cudatoolkit}
     '';
   }