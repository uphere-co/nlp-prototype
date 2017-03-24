{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay
}:

with pkgs;

let hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") {
                 inherit pkgs;
               };
    
    newHaskellPackages = haskellPackages.override {
      overrides = self: super: 
        (hsconfig self super // 
          { "query-common" = self.callPackage (import ../query-common) { }; 
          });
    };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              aeson-pretty
              base64-bytestring
              haskeline
              http-client
              http-client-tls
              http-conduit
              monad-loops
              servant
              servant-server
              text-format
              distributed-process
              distributed-process-lifted
              query-common
	      network-transport-uphere
	      monad-loops uuid aeson
	      cabal-install conduit conduit-extra
	      base64-bytestring aeson
	      http-client http-client-tls
	      network-simple

              optparse-applicative
            ]);
            
in stdenv.mkDerivation {
  name = "intrinio-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
