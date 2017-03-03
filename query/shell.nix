{ pkgs ? import <nixpkgs> {}
, rnnpp ? import ../rnn++ {}
}:

with pkgs;

let toolz_cpp = callPackage ../nix/default-cpp.nix {};
    hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };

    queryBindingSrc = import ../query-binding {
      inherit pkgs;
      haskellPackages = haskellPackages.override { overrides = hsconfig; };
    }; 
    hsconfig2= self: super: {
      query-common = self.callPackage ../query-common {};
      "query-binding" = self.callPackage
        ({ mkDerivation, base, fficxx, fficxx-runtime, stdenv
         , template-haskell, rnnpp, json, hdf5-cpp, msgsl, tbb
         , fmt, pqxx
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
	     rnnpp json hdf5-cpp msgsl tbb variant fmt pqxx elfutils
	   ];
	   license = stdenv.lib.licenses.bsd3;
	 }) { inherit rnnpp tbb hdf5-cpp;
              json  = toolz_cpp.json;
              msgsl = toolz_cpp.msgsl;
              variant = toolz_cpp.variant;
              fmt = toolz_cpp.fmt;
              pqxx = libpqxx;
              inherit elfutils;
            };
    };
    
    newHaskellPackages = haskellPackages.override { overrides = self: super: hsconfig self super // hsconfig2 self super; };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              distributed-process
	      #network-transport-zeromq
	      network-transport-uphere
	      monad-loops uuid aeson
	      cabal-install conduit conduit-extra
	      base64-bytestring aeson
	      http-client http-client-tls
	      hdf5 query-common
	      network-simple
	      fficxx-runtime
	      fficxx
              query-binding
            ]);
in stdenv.mkDerivation {
  name = "query-dev";
  buildInputs = [ hsenv hdf5-cpp liblbfgs cppzmq zeromq libpqxx elfutils
		  rnnpp tbb
		  toolz_cpp.fmt
                  toolz_cpp.msgsl
		  toolz_cpp.json
                  toolz_cpp.spdlog
		  toolz_cpp.xxhashct
                  toolz_cpp.backwardcpp
                  toolz_cpp.variant
                ];
  shellHook = ''
  '';
}
