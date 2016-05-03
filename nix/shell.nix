{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    hsconfig = self: super: {
      accelerate = self.callPackage
	({ mkDerivation, array, base, base-orphans, containers, directory
	, exceptions, fclabels, filepath, ghc-prim, hashable, hashtables
	, mtl, pretty, stdenv, template-haskell, transformers, unique, unix
	, unordered-containers
	}:
	mkDerivation {
	  pname = "accelerate";
	  version = "2.0.0.0";
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
	  src = fetchgit {
	    url = "git://github.com/wavewave/accelerate-cuda.git";
	    rev = "cc5d2a88de96ff30df3ddab8d85f4e4465069e60";
	    sha256 = "1dnz4z5m0dz5k8nhvbls42rr27ggrwg9bfl9p9v78cj5x17h84bc";
	  };
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
    
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              xml-conduit split unordered-containers vector-algorithms storable-tuple
	      tagged either
              accelerate
              accelerate-io
	      accelerate-cuda
	      mersenne-random
	      math-functions
            ]);
in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv cudatoolkit ];
     shellHook = ''
       export CUDA_PATH=${pkgs.cudatoolkit}
     '';
   }