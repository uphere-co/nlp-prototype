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
}:

with pkgs;

let

    hsconfig1 = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    queryBindingSrc = import ../query-binding {
      inherit pkgs;
      haskellPackages = haskellPackages.override { overrides = hsconfig1; };
    }; 

    hsconfig2 = self: super: {
      "query-common" = self.callPackage ../query-common { };
      "query" = self.callPackage (import ./default.nix) {
        inherit hdf5_cpp tbb rnnpp json elfutils fmt msgsl spdlog xxhashct backwardcpp ;
	pqxx = libpqxx;
      };
      "query-binding" = self.callPackage
        ({ mkDerivation, base, fficxx, fficxx-runtime, stdenv
         , template-haskell
         }:
         mkDerivation {
           pname = "query-binding";
           version = "0.0";
           src = queryBindingSrc;
           libraryHaskellDepends = [
	     base fficxx fficxx-runtime template-haskell
	   ];
	   license = stdenv.lib.licenses.bsd3;
	 }) { };
    };
    hsconfig = self: super: (hsconfig1 self super // hsconfig2 self super); 
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
in newhaskellPackages.query
