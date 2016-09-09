{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ./default-cpp.nix { };
    toolz_py = callPackage ./default-python.nix {
                           inherit pkgs;
                           buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
                           };
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
          gtest
          eigen
          
                  cmake clang_38 clang-analyzer
                  hdf5 hdf5-cpp zeromq
                  tbb 
                  linuxPackages_4_6.perf
                  #openblas
                ]
                ++
                (with python27Packages;
                [
                toolz_py.nltk
                numpy scipy scikitlearn cython
                ]);
  shellHook = ''
    export MS_GSL=${toolz.msgsl}
    export SPDLOG=${toolz.spdlog}
    export LIBSVM=${toolz.libsvm}
    export LIBLINEAR=${toolz.liblinear}
    export PATH=$LIBLINEAR/liblinear:$LIBSVM/libsvm:$PATH
  '';
}
