{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let
    llvmGeneralSrc = fetchgit {
      url = "git://github.com/wavewave/llvm-general.git";
      rev = "646bfa6168430b56035f9858c9676ac22fba4976";
      sha256 = "1g97vi8jmp7n783s0kbk5vhrh4kjqf619nhw3qxjjlpz08vhjr9m";
    };

    hsconfig = self: super: {
      "lbfgs" = self.callPackage
        ({ mkDerivation, array, base, stdenv, vector }:
         mkDerivation {
           pname = "lbfgs";
           version = "0.0.999";
           src = fetchgit {
             url = "git://github.com/wavewave/lbfgs-hs.git";
             rev = "13414397880fa35ddc5eeb87d3b7720e4023f8fa";
             sha256 = "00zv90vn1krva7jzjszjlbqypf4dzhrnvgbw6d1l071zxxipad8v";
           };
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

    };
    
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              xml-conduit split unordered-containers vector-algorithms storable-tuple
              tagged either
              mersenne-random
              math-functions
              hblas
              lbfgs
              MemoTrie lens
              language-c containers
	      llvm-general
	      QuickCheck
	      tasty
	      tasty-golden
	      tasty-hunit
	      tasty-quickcheck
	      tasty-smallcheck
              zenc              
            ]);

    #llvm_35_debug = llvm_35.override { debugVersion = true; }; 

in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv graphviz #llvm_38
		   ];
     shellHook = ''
     '';
   }

