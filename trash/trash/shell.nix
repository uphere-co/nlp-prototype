{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let
    toolz = callPackage ../nix/default-cpp.nix { };

    hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };

    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
	      conduit-extra
              hmatrix
	      hmatrix-svdlibc
	      #histogram-fill
	      monad-loops
	      resourcet
	      shell-conduit
	      text
	      unordered-containers
            ]);

in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv toolz.armadillo boost hello ];
     shellHook = ''
     '';
   }
