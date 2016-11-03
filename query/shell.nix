{ pkgs ? import <nixpkgs> {}
, rnnpp ? import ../rnn++ {}
, query-common ? import ../query-common {}
}:

with pkgs;

let toolz_cpp = callPackage ../nix/default-cpp.nix {};
    hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    newHaskellPackages = haskellPackages.override { overrides = hsconfig; };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              distributed-process
	      network-transport-zeromq
	      monad-loops uuid aeson
	      cabal-install conduit conduit-extra
	      base64-bytestring aeson
	      http-client http-client-tls
	      hdf5 query-common
            ]);
in stdenv.mkDerivation {
  name = "query-dev";
  buildInputs = [ hsenv hdf5-cpp liblbfgs cppzmq zeromq libpqxx
		  rnnpp tbb
		  toolz_cpp.fmt
                  toolz_cpp.msgsl
		  toolz_cpp.json
                ];
  shellHook = ''
  '';
}
