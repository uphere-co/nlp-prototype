{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    hsconfig2 = self: super: {
      "conduit" = haskell.lib.overrideCabal super.conduit (drv: {
        version = "1.2.9";
        sha256 = "01ng2k944hc258q3an4s1a6ym7g9j8fcr0jcdkwapdyw2s4rvpwa";
        libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.split ];
      });
    };

    newHaskellPackages = haskellPackages.override {
      overrides = self: super: hsconfig self super // hsconfig2 self super;
    };

    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              aeson conduit conduit-extra #distributed-process distributed-process-async
              either monad-loops orc split text-format
            ]);
in stdenv.mkDerivation {
  name = "wikidata-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
     PS1="\n\[\033[0;34m\][\u@\h.wikidata:\w]\$\[\033[0m\] "  
  '';
}
