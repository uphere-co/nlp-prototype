{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    hsconfig = self: super: {
    };
    
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              curl
              HTTP
              tagsoup
              ]);

in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv
                   ];
     shellHook = ''
     '';
   }
