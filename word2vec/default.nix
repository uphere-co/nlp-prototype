{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ../nix/default-cpp.nix { };
    rnnppsrc = srcOnly {
      name = "rnn++-src";
      src = ../rnn++;
    }; 
in
stdenv.mkDerivation rec {
  version = "0.0";
  name = "word2vec-${version}";
  src = ./.;
  buildInputs = [ boost
                  toolz.openblas_static

                  toolz.armadillo toolz.libpca
		  toolz.msgsl
		  toolz.spdlog

		  pkgconfig
                  cmake clang_38 
                  hdf5 hdf5-cpp 
                  tbb

                ];
  cmakeFlags = "-DRNNPP_PATH=${rnnppsrc}";
  installPhase = ''
  '';
}
