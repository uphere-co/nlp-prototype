{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ./default-cpp.nix { };
in
stdenv.mkDerivation {
  name = "cpp-env";
  buildInputs = [ boost
                  toolz.openblas_static
                  toolz.armadillo toolz.libpca
		          toolz.msgsl
    	          toolz.spdlog
                  gdb
		          pkgconfig
          		  gfortran
                  cmake clang_38 clang-analyzer
                  hdf5 hdf5-cpp zeromq
                  tbb 
                  linuxPackages_4_6.perf
                ];
  shellHook = ''
    export MS_GSL=${toolz.msgsl}
    export SPDLOG=${toolz.spdlog}
  '';
}
