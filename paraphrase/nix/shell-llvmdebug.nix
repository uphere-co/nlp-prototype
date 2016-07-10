{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;
#with import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; };


let 
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
 

      cuda = self.callPackage
        ({ mkDerivation, base, bytestring, c2hs, pretty, stdenv
         , template-haskell
         , cudatoolkit, autoconf
         }:
         mkDerivation {
           pname = "cuda";
           version = "0.7.1.0";
           src = fetchgit {
             url = "git://github.com/wavewave/cuda.git";
             rev = "05145a79dc652a1a9f72bc0d99a6db59a9dd55df";
             sha256 = "16hsf2qwzai04bf0gyn6wpw37fk0kldbm6gcx1zzlmhkvb5g94wm";
           };
           isLibrary = true;
           isExecutable = true;
           libraryHaskellDepends = [ base bytestring template-haskell ];
           libraryToolDepends = [ c2hs ];
           executableHaskellDepends = [ base pretty ];
           librarySystemDepends = [ cudatoolkit ];
           buildDepends = [ autoconf ];
           extraLibraries = (super.extraLibraries or []) ++ [pkgs.linuxPackages.nvidia_x11];
           configureFlags = (super.configureFlags or []) ++
             pkgs.lib.optional pkgs.stdenv.is64bit "--extra-lib-dirs=${cudatoolkit}/lib64" ++ [
               "--extra-lib-dirs=${cudatoolkit}/lib"
               "--extra-include-dirs=${cudatoolkit}/include"
             ];
           preConfigure = ''
             unset CC
           '';
           homepage = "https://github.com/tmcdonell/cuda";
           description = "FFI binding to the CUDA interface for programming NVIDIA GPUs";
           license = stdenv.lib.licenses.bsd3;
         }) {};

      cublas = self.callPackage
        ({ mkDerivation, base, cuda, filepath, language-c
         , stdenv, storable-complex, template-haskell, autoconf
         , cudatoolkit
         }:
         mkDerivation {
           pname = "cublas";
           version = "0.3.0.0";
           src = fetchgit {
             url = "git://github.com/wavewave/cublas.git";
             rev = "0ad62bcfcbee369a03ddbf292e49514fb56a2886";
             sha256 = "05rspmdl3q2ppgkfzhwviyphi82262r21h9142m1fg3dxiz22k1b";
           };
           libraryHaskellDepends = [
             base cuda filepath language-c storable-complex template-haskell
           ];
           buildDepends = [ autoconf ];
           librarySystemDepends = [ cudatoolkit ];
           preConfigure = ''
             unset CC
             sed -i -e 's|/usr/bin/gcc|gcc|g' Foreign/CUDA/Cublas/THBase.hs 
           '';
           jailbreak = true;
           homepage = "https://github.com/bmsherman/cublas";
           description = "FFI bindings to the CUDA CUBLAS and CUSPARSE libraries";
           license = stdenv.lib.licenses.bsd3;
         }) {};
    
      accelerate = self.callPackage
        ({ mkDerivation, array, base, base-orphans, containers, directory
        , exceptions, fclabels, filepath, ghc-prim, hashable, hashtables
        , mtl, pretty, stdenv, template-haskell, transformers, unique, unix
        , unordered-containers
        }:
        mkDerivation {
          pname = "accelerate";
          version = "2.0.0.0";
          #src = /home/wavewave/repo/srcc/accelerate;
          src = fetchgit {
            url = "git://github.com/AccelerateHS/accelerate.git";
            rev = "9d0118f8ef3cd47d64a737b9d7d1d902a5ee72b7";
            sha256 = "0gvqv6vx4izwhlgzs13vlmsp07lnfzpy2nzw5ahla8fkxmayllq0";
          };
          libraryHaskellDepends = [
            array base base-orphans containers directory exceptions fclabels
            filepath ghc-prim hashable hashtables mtl pretty template-haskell
            transformers unique unix unordered-containers
          ];
          doHaddock = false;
          doCheck = false;
          
          homepage = "https://github.com/AccelerateHS/accelerate/";
          description = "An embedded language for accelerated array processing";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      accelerate-io = self.callPackage 
        ({ mkDerivation, accelerate, array, base, bmp, bytestring, repa
        , stdenv, vector
        }:
        mkDerivation {
          pname = "accelerate-io";
          version = "2.0.0.0";
          src = fetchgit {
            url = "git://github.com/AccelerateHS/accelerate-io.git";
            rev = "a5e73442fd987243de12bfb0c1ff7a87d701ea94";
            sha256 = "0rsr76y54r3dsrk39lbps660z4lvn3lrxp38ls13nr5xlf11aw7p";
          };
          libraryHaskellDepends = [
            accelerate array base bmp bytestring repa vector
          ];
          homepage = "https://github.com/AccelerateHS/accelerate-io";
          description = "Read and write Accelerate arrays in various formats";
          license = stdenv.lib.licenses.bsd3;
        }) {};
      accelerate-cuda = self.callPackage
        ({ mkDerivation, accelerate, array, base, binary, bytestring
        , containers, cryptohash, cuda, directory, fclabels, filepath
        , hashable, hashtables, language-c-quote, mainland-pretty, mtl
        , old-time, pretty, process, SafeSemaphore, srcloc, stdenv
        , template-haskell, text, transformers, unix, unordered-containers
        }:
        mkDerivation {
          pname = "accelerate-cuda";
          version = "2.0.0.0";
          #src = /home/wavewave/repo/srcc/accelerate-cuda;
          src = fetchgit {
            url = "https://github.com/AccelerateHS/accelerate-cuda.git";
            rev = "b33150316aa8b8dd611cfc59f08429bd64fdb747";
            sha256 = "1b8784bn9gqidrhc9z8bnvxmvp7cp5sq45jp6907gpa1c4jnq3l8";
          };
          configureFlags = "-fdebug";
          doCheck = false;
          doHaddock = false;
          libraryHaskellDepends = [
            accelerate array base binary bytestring containers cryptohash cuda
            directory fclabels filepath hashable hashtables language-c-quote
            mainland-pretty mtl old-time pretty process SafeSemaphore srcloc
            template-haskell text transformers unix unordered-containers
          ];
          homepage = "https://github.com/AccelerateHS/accelerate-cuda/";
          description = "Accelerate backend for NVIDIA GPUs";
          license = stdenv.lib.licenses.bsd3;
        }) {};
    };
    
    newhaskellPackages = haskell.packages.ghc7103.override { overrides = hsconfig; };
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              xml-conduit split unordered-containers #vector-algorithms
	      storable-tuple
              tagged either
              mersenne-random
              math-functions
              hblas
              lbfgs
              MemoTrie lens
              language-c containers
	      llvm-general
	      QuickCheck
	      test-framework
	      test-framework-hunit
	      test-framework-quickcheck2
            ]);

    llvm_35_debug = llvm_35.override { debugVersion = true; }; 

in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv graphviz llvm_35_debug ];
     shellHook = ''
     '';
   }
