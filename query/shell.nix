{ pkgs ? import <nixpkgs> {}
, rnnpp ? import ../rnn++ {}
}:

with pkgs;

let toolz_cpp = callPackage ../nix/default-cpp.nix {};
    hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    hsconfig2= self: super: {
      query-common = self.callPackage ../query-common {};
    };
    
    newHaskellPackages = haskellPackages.override { overrides = self: super: hsconfig self super // hsconfig2 self super; };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              distributed-process
	      network-transport-zeromq
	      monad-loops uuid aeson
	      cabal-install conduit conduit-extra
	      base64-bytestring aeson
	      http-client http-client-tls
	      hdf5 query-common
	      hedis
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
