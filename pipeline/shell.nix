{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let hsenv = haskellPackages.ghcWithPackages (p: with p; [
          aeson
          cabal-install
	      cassava
          conduit
          conduit-extra
	      lens
          monad-loops
          split
	      data-default
	    ]);
in
stdenv.mkDerivation {
  name = "pipeline-env";
  buildInputs =  [
		         hsenv
                 ];
  shellHook = ''
  '';
}
