{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };

    newHaskellPackages = haskellPackages.override { overrides = self: super: hsconfig self super; };

    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              aeson either
            ]);
in stdenv.mkDerivation {
  name = "wikidata-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
     PS1="\n\[\033[0;34m\][\u@\h.wikidata:\w]\$\[\033[0m\] "  
  '';
}
