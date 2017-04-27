{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay
}:

with pkgs;

let toolz_cpp = callPackage (uphere-nix-overlay + "/nix/default-cpp.nix") { };
    config = import ./config.nix { inherit pkgs toolz_cpp; };
    # mystdenv = clangStdenv; # clangStdenv has clang 3.9 as of 20170427.
    mystdenv = llvmPackages_4.stdenv;
in
mystdenv.mkDerivation rec {
  version = "0.0";
  name = "rnn++-${version}";
  src = ./.;
  buildInputs = config; 
  enableParallelBuilding = true;
}
