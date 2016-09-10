{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz_cpp = callPackage ../nix/default-cpp.nix { };
in
stdenv.mkDerivation rec {
  version = "0.0";
  name = "rnn++-${version}";
  src = ./.;
  buildInputs = [ cmake clang_38 
                  boost
                  hdf5 hdf5-cpp liblbfgs 
                  tbb openblas  
                  toolz_cpp.msgsl
                  toolz_cpp.spdlog
		  toolz_cpp.fmt
                ];
  enableParallelBuilding = true;
}
