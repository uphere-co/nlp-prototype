{ pkgs
, uphere-nix-overlay
, hdf5_cpp
, tbb
, rnnpp
, json
, libpqxx
, elfutils
, flatbuffers
, fmt
, msgsl
, spdlog
, xxhashct
, backwardcpp
, variant
}:

let
  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { inherit pkgs; };

  queryBindingSrc = import ../query-binding {
    inherit pkgs;
    haskellPackages = pkgs.haskellPackages.override { overrides = hsconfig; };
  }; 

  newconfig = self: super: {
    "query-common" = self.callPackage ../query-common { };
    "query" = self.callPackage (import ./default.nix) { };
    "query-binding" = self.callPackage
      ({ mkDerivation, base, fficxx, fficxx-runtime, stdenv
       , template-haskell, rnnpp, json, hdf5_cpp, msgsl, tbb
       , flatbuffers, fmt, pqxx
       , variant
       , elfutils
       }:
       mkDerivation {
         pname = "query-binding";
         version = "0.0";
         src = queryBindingSrc;
         libraryHaskellDepends = [
           base fficxx fficxx-runtime template-haskell
         ];
         librarySystemDepends = [
           rnnpp json hdf5_cpp msgsl tbb variant flatbuffers fmt pqxx elfutils
         ];
         license = stdenv.lib.licenses.bsd3;
       }) { inherit rnnpp json hdf5_cpp msgsl tbb variant flatbuffers fmt;
            pqxx = libpqxx;
          };
  };

in

self: super:
  hsconfig self super // newconfig self super