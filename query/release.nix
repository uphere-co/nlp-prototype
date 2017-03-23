{ pkgs ? (import <nixpkgs>{})
, uphere-nix-overlay
, hdf5_cpp
, tbb
, rnnpp
, json
, libpqxx
, elfutils
, fmt
, msgsl
, spdlog
, xxhashct
, backwardcpp
, variant
}:

with pkgs;

let
    config = import ./config.nix { inherit pkgs uphere-nix-overlay hdf5_cpp tbb rnnpp json libpqxx elfutils fmt msgsl spdlog xxhashct backwardcpp variant; };
    newhaskellPackages = haskellPackages.override { overrides = config; };
    
in newhaskellPackages.query
