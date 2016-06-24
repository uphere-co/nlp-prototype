{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ./default-cpp.nix { };
in
stdenv.mkDerivation {
  name = "cpp-env";
  buildInputs = [ gcc boost openblas #liblapack
  	      	   toolz.armadillo toolz.libpca
		   gdb
                ];
  shellHook = ''
  '';
}