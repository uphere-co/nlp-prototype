{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay
, rnnpp ? import ../rnn++ {}
}:

with pkgs;

let toolz_cpp = callPackage ../nix/default-cpp.nix {};

    config = import ./config.nix {
               inherit pkgs uphere-nix-overlay;
               hdf5_cpp = hdf5-cpp;
               inherit (toolz_cpp) json fmt xxhashct backwardcpp variant msgsl; 
               inherit tbb rnnpp libpqxx elfutils spdlog;

             };

    newHaskellPackages = haskellPackages.override { overrides = config; };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              distributed-process distributed-process-lifted
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
              optparse-applicative
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
