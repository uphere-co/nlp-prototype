{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let
    llvmGeneralSrc = fetchgit {
      url = "git://github.com/wavewave/llvm-general.git";
      rev = "646bfa6168430b56035f9858c9676ac22fba4976";
      sha256 = "1g97vi8jmp7n783s0kbk5vhrh4kjqf619nhw3qxjjlpz08vhjr9m";
    };
    fficxxSrc = fetchgit {
      url = "git://github.com/wavewave/fficxx.git";
      rev = "d6c12bf0ffc057eb5a2979352d5212bc68da1c2d";
      sha256 = "0d6rqlg3w5zb59c7ckbbhphyl7l7iks44c8rhaia8qiva8h6y20y";
    };

in self: super: {
      "lbfgs" = self.callPackage
        ({ mkDerivation, array, base, stdenv, vector }:
         mkDerivation {
           pname = "lbfgs";
           version = "0.1";
           libraryHaskellDepends = [ array base vector ];
           description = "L-BFGS optimization";
           license = "unknown";
         }) {};
      "hblas" = self.callPackage
        ({ mkDerivation, base, openblas, HUnit, liblapack, primitive
         , storable-complex, tasty, tasty-hunit, vector
         }:
         mkDerivation {
           pname = "hblas";
           version = "0.3.2.1";
           sha256 = "3e159cc8c98735861edad47cd4da11bd5862bb629601a9bc441960c921ae8215";
           revision = "1";
           editedCabalFile = "cf7946aba77f6f23a665fe06859a6ba306b513f5849f9828ed171e84bad4a43e";
           libraryHaskellDepends = [ base primitive storable-complex vector ];
           librarySystemDepends = [ openblas liblapack ];
           testHaskellDepends = [ base HUnit tasty tasty-hunit vector ];
           jailbreak = true;
           doCheck = false;
           configureFlags = ["-fOpenBLAS"];
           homepage = "http://github.com/wellposed/hblas/";
           description = "Human friendly BLAS and Lapack bindings for Haskell";
           license = stdenv.lib.licenses.bsd3;
           hydraPlatforms = stdenv.lib.platforms.none;
         }) { inherit (pkgs) liblapack;};

      llvm-general-pure =
        let p1 = haskell.lib.overrideCabal super.llvm-general-pure (drv: {
                   src = "${llvmGeneralSrc}/llvm-general-pure"; 
                 });
        in haskell.lib.addBuildDepend p1 self.transformers-compat; 

      llvm-general = haskell.lib.overrideCabal super.llvm-general (drv: {
        src = "${llvmGeneralSrc}/llvm-general";
        libraryToolDepends = [ llvm_38 ];
      });

      "bindings-hdf5" = self.callPackage
        ({ mkDerivation, base, bindings-DSL, bytestring, hdf5, libffi
         , monad-peel, primitive, stdenv, transformers, vector
         }:
	 mkDerivation {
           pname = "bindings-hdf5";
           version = "1.8.12";
	   #src = /home/wavewave/repo/srcc/bindings-hdf5;
           src = fetchgit {
             url = "git://github.com/wavewave/bindings-hdf5.git";
             rev = "881048b816789a89b6215ee566496425d37a739d";
             sha256 = "09y7hk91854k6iz3l9n4kwsvi3q8phl7ydnwkdhz8fl96xb79235";
	   };
           libraryHaskellDepends = [
             base bindings-DSL bytestring libffi monad-peel primitive
             transformers vector
           ];
           libraryPkgconfigDepends = [ hdf5 ];
           homepage = "https://github.com/mokus0/bindings-hdf5";
           description = "Bindings to HDF5";
           license = stdenv.lib.licenses.publicDomain;
         }) { hdf5 = pkgs.hdf5; };
	 
      "hdf5" = self.callPackage
        ({ mkDerivation, base, bindings-hdf5, bytestring, stdenv, tagged
         , vector
         }:
         mkDerivation {
           pname = "hdf5";
           version = "1.8.8";
           src = fetchgit {
             url = "git://github.com/wavewave/hs-hdf5.git";
             rev = "196e0d714a34e97358be9a907af0b93b4c7922e3";
             sha256 = "1gwrcdza3qsw3hq8jsvp7bgxr8fzkg0jjb15pmhnrn28jkhlhx03";
	   };
           libraryHaskellDepends = [
             base bindings-hdf5 bytestring tagged vector
           ];
           homepage = "https://github.com/mokus0/hs-hdf5";
           description = "Haskell interface to the HDF5 scientific data storage library";
           license = stdenv.lib.licenses.publicDomain;
         }) {};
      "network-transport-uphere" = self.callPackage
        ({ mkDerivation, base, binary, bytestring, containers, data-accessor, distributed-process
         , network, network-simple, network-transport, network-transport-tests, stm, either
	 , stdenv
	 }:
	 mkDerivation {
	   pname = "network-transport-uphere";
	   version = "0.0";
	   src = fetchgit {
             url = "git://github.com/uphere-co/network-transport-uphere.git";
             rev = "5f53fb37799ca00de2e575a6ba68781759c58eb4";
             sha256 = "1vgrp6x5480p9vgk1ci7qydv76r76742inm5mzvqgxsvnyv5hj77";
	   };
	   libraryHaskellDepends = [
	     base bytestring containers data-accessor network network-transport
	   ];
	   executableHaskellDepends = [
	     base binary distributed-process network-simple network-transport either stm
	   ];
	   homepage = "http://haskell-distributed.github.com";
	   description = "UpHere specific network transport";
	   license = stdenv.lib.licenses.bsd3;
	   doCheck = false;
	 }) {};
	 
      "fficxx-runtime" = self.callPackage
        ({ mkDerivation, base, stdenv, template-haskell }:
	 mkDerivation {
           pname = "fficxx-runtime";
           version = "0.2.999";
           src = "${fficxxSrc}/fficxx-runtime";
           libraryHaskellDepends = [ base template-haskell ];
           description = "Runtime for fficxx-generated library";
           license = stdenv.lib.licenses.bsd3;
         }) {};
      "fficxx" = self.callPackage
        ({ mkDerivation, base, bytestring, Cabal, containers, data-default
         , directory, either, errors, filepath, hashable, haskell-src-exts
         , lens, mtl, process, pureMD5, split, stdenv, template
         , template-haskell, text, transformers, unordered-containers
         }:
         mkDerivation {
           pname = "fficxx";
           version = "0.2.999";
           src = "${fficxxSrc}/fficxx";
           libraryHaskellDepends = [
             base bytestring Cabal containers data-default directory either
             errors filepath hashable haskell-src-exts lens mtl process pureMD5
             split template template-haskell text transformers
             unordered-containers
           ];
           description = "automatic C++ binding generation";
           license = stdenv.lib.licenses.bsd3;
         }) {};
      

    }
