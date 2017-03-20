{ pkgs ? (import <nixpkgs>{})
, uphere-nix-overlay
}:

with pkgs;

let 

  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { inherit pkgs; };
  newhaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // { query-common = self.callPackage (import ./default.nix) {}; };
  };
    
in

newhaskellPackages.query-common
