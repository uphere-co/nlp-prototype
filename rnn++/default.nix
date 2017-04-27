{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay
}:

with pkgs;

let toolz_cpp = callPackage (uphere-nix-overlay + "/nix/default-cpp.nix") { };
    config = import ./config.nix { inherit pkgs toolz_cpp; };
in
stdenv.mkDerivation rec {
  version = "0.0";
  name = "rnn++-${version}";
  src = ./.;
  buildInputs = config; 
  enableParallelBuilding = true;
}
