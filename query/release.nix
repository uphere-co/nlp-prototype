{ pkgs ? (import <nixpkgs>{})
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

    hsconfig1 = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };

    config = import ./config.nix { inherit pkgs hdf5_cpp tbb rnnpp json libpqxx elfutils fmt msgsl spdlog xxhashct backwardcpp variant; };
    hsconfig = self: super: (hsconfig1 self super // config self super); 
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
in newhaskellPackages.query
