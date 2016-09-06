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
		  toolz.msgsl
		  toolz.spdlog
          toolz.libsvm
          toolz.liblinear
          #libsvm
                  gdb
		  pkgconfig
		  gfortran #stdenv.cc.libc
		  gnuplot
          python27Full

                  cmake clang_38 clang-analyzer
                  hdf5 hdf5-cpp zeromq
                  tbb 
                  linuxPackages_4_6.perf
                  #openblas
                ];
  shellHook = ''
    export MS_GSL=${toolz.msgsl}
    export SPDLOG=${toolz.spdlog}
    export LIBSVM=${toolz.libsvm}
    export LIBLINEAR=${toolz.liblinear}
    export PATH=$LIBLINEAR/liblinear:$LIBSVM/libsvm:$PATH
  '';
}
