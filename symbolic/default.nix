{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    hsconfig1 = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    hsconfig2 = self: super: { 
      symbolic = self.callPackage
        ({ mkDerivation, array, base, binary, bytestring, containers, either
	 , hashable, language-c, lens, llvm-general, llvm-general-pure
	 , MemoTrie, mtl, pretty, stdenv, tasty, tasty-hunit
	 , tasty-quickcheck, tasty-smallcheck, transformers
	 , unordered-containers, vector, zenc
	 }:
	 mkDerivation {
	   pname = "symbolic";
	   version = "0.0";
	   src = ./.;
	   isLibrary = true;
	   isExecutable = true;
	   libraryHaskellDepends = [
	     array base binary bytestring containers either hashable language-c
	     lens llvm-general llvm-general-pure MemoTrie mtl pretty
	     transformers unordered-containers vector zenc
	   ];
	   executableHaskellDepends = [
	     base containers hashable llvm-general llvm-general-pure MemoTrie
	     mtl transformers unordered-containers vector
	   ];
	   testHaskellDepends = [
	     base hashable MemoTrie tasty tasty-hunit tasty-quickcheck
	     tasty-smallcheck unordered-containers
	   ];
	   homepage = "https://github.com/uphere-co/nlp-prototype";
	   description = "Symbolic computation";
	   license = stdenv.lib.licenses.bsd3;
	 }) {};
    };
    hsconfig = self: super: (hsconfig1 self super // hsconfig2 self super); 
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
in newhaskellPackages.symbolic