{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };

    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              hmatrix
	      #histogram-fill
	      text
	      unordered-containers
            ]);

in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv graphviz ];
     shellHook = ''
     '';
   }
