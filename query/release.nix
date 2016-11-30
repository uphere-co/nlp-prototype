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
, backwardcpp
}:

with pkgs;

let 
    hsconfig1 = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    hsconfig2 = self: super: {
      query-common = self.callPackage ../query-common {
      };
      query = self.callPackage (import ./default.nix) {
        inherit hdf5_cpp tbb rnnpp json elfutils fmt msgsl spdlog backwardcpp;
	pqxx = libpqxx;
      };
    };
    hsconfig = self: super: (hsconfig1 self super // hsconfig2 self super); 
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
in newhaskellPackages.query
