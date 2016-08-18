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
		  gnuplot

                  cmake clang_38 clang-analyzer
                  hdf5 hdf5-cpp zeromq
                  tbb 
                  linuxPackages_4_6.perf
                  #openblas
                ];
  shellHook = ''
  '';
}
