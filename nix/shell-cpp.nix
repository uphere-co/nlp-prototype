{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ./default-cpp.nix { };
in
stdenv.mkDerivation {
  name = "cpp-env";
  buildInputs = [ boost
                  toolz.openblas_static
                  #liblapack
                  toolz.armadillo toolz.libpca
                  gdb
		  pkgconfig
		  gfortran #stdenv.cc.libc
                ];
  shellHook = ''
  '';
}