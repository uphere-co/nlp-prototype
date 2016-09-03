{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ../nix/default-cpp.nix { };
in
stdenv.mkDerivation rec {
  version = "0.0";
  name = "word2vec-${version}";
  src = ./.;
  buildInputs = [ boost
                  toolz.openblas_static
                  #liblapack
                  toolz.armadillo toolz.libpca
		  toolz.msgsl
		  toolz.spdlog
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
  installPhase = ''
  '';
}
