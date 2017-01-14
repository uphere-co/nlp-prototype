{ pkgs }:



with pkgs;


let
  openblas_static = callPackage (
    { stdenv, fetchurl, gfortran, perl, which, config, coreutils
    # Most packages depending on openblas expect integer width to match pointer width,
    # but some expect to use 32-bit integers always (for compatibility with reference BLAS).
    , blas64 ? null
    }:

    with stdenv.lib;

    let blas64_ = blas64; in

    let local = config.openblas.preferLocalBuild or false;
	binary =
	  { i686-linux = "32";
	    x86_64-linux = "64";
	    x86_64-darwin = "64";
	  }."${stdenv.system}" or (throw "unsupported system: ${stdenv.system}");
	genericFlags =
	  [ "DYNAMIC_ARCH=1"
            "NUM_THREADS=64"
            #"BUILD_STATIC_LIBS=1"
          ];
	localFlags = config.openblas.flags or
	  optionals (hasAttr "target" config.openblas) [ "TARGET=${config.openblas.target}" ];
	blas64 = if blas64_ != null then blas64_ else hasPrefix "x86_64" stdenv.system;

	version = "0.2.17";
    in
    stdenv.mkDerivation {
      name = "openblas-${version}";
      src = fetchurl {
	url = "https://github.com/xianyi/OpenBLAS/archive/v${version}.tar.gz";
	sha256 = "1gqdrxgc7qmr3xqq4wqcysjhv7ix4ar7ymn3vk5g97r1xvgkds0g";
	name = "openblas-${version}.tar.gz";
      };

      inherit blas64;

      nativeBuildInputs = optionals stdenv.isDarwin [coreutils] ++ [gfortran perl which];

      makeFlags =
	(if local then localFlags else genericFlags)
	++
	optionals stdenv.isDarwin ["MACOSX_DEPLOYMENT_TARGET=10.9"]
	++
	[
	  "FC=gfortran"
	  # Note that clang is available through the stdenv on OSX and
	  # thus is not an explicit dependency.
	  "CC=${if stdenv.isDarwin then "clang" else "gcc"}"
	  ''PREFIX="''$(out)"''
	  "BINARY=${binary}"
	  #"USE_OPENMP=${if stdenv.isDarwin then "0" else "1"}"
	  #"INTERFACE64=${if blas64 then "1" else "0"}"
	];

      doCheck = true;
      checkTarget = "tests";

      meta = with stdenv.lib; {
	description = "Basic Linear Algebra Subprograms";
	license = licenses.bsd3;
	homepage = "https://github.com/xianyi/OpenBLAS";
	platforms = platforms.unix;
	maintainers = with maintainers; [ ttuegel ];
      };
    }
  ) {};

in 

rec {
    inherit openblas_static;
    
    armadillo = stdenv.mkDerivation rec {
      name = "armadillo-7.400.2";
      src = pkgs.fetchurl {
        url = "http://sourceforge.net/projects/arma/files/${name}.tar.xz";
        sha256 = "0xmpnqhm9mwr1lssjyarj0cl8b4svbqv6z1xa1dxlwd2ly1srkg4";
      };
      buildInputs = [ cmake ]; 
      propagatedBuildInputs = [ openblas_static liblapack ]; 
      meta = {
      };
    };

    libpca = stdenv.mkDerivation rec {
      name = "libpca-1.3.3";
      src = pkgs.fetchurl {
        url = "https://sourceforge.net/projects/libpca/files/${name}.tar.gz";
	sha256 = "0f42lhnnmc83vxj854xb49p4ha2nk7mdwzlb7fss7cxnkwrkp75w";
      };
      propagatedBuildInputs = [ armadillo ];
      
      meta = {
      
      };
    };

    msgsl = stdenv.mkDerivation rec {
      name = "GSL-${version}";
      version = "r0535138";
      src = fetchgit {
        url = "https://github.com/Microsoft/GSL.git";
	rev = "0535138459d0f78e39a2e558bc239f5727eaa13c";
	sha256 = "0gvkdswa7hivw2as8g1zda79fbs4akd19j4dddkzdd31d8riz5jj";
      };
      buildInputs = [ cmake ];
      patches = [ ];

      installPhase = ''
        make install 
        mkdir -p $out/include/gsl
        cp -a ../include/* $out/include/gsl/
      '';
    };


    spdlog = stdenv.mkDerivation rec {
      name = "spdlog";
      version = "1.0";
      src = fetchgit {
        url = "https://github.com/gabime/spdlog.git";
	rev = "f310cc460f043990fe03d2e5c018cbc1779057dc";
	sha256 = "0c0kv5mrwd0gyvl9jgg8b0rym1x9x7c8vaha0bcajlp6akhhgwfv";
      };
      buildInputs = [ cmake ];
      patches = [ ];
    };

    fmt = stdenv.mkDerivation rec {
      name = "fmt-${version}";
      version = "0d25f6f";
      src = fetchgit {
        url = "https://github.com/fmtlib/fmt.git";
	rev = "0d25f6fcbbf0a867b939a5501965ee4462b21ee6";
	sha256 = "179qcshc7v125lc1cgy1sixczrpjvg4grnqxwjxx8gwm4g46bvm8";
      };
      buildInputs = [ cmake ];
      cmakeFlags = ["-DBUILD_SHARED_LIBS=TRUE"];
      patches = [ ];
      enableParallelBuilding = true;
      checkPhase = ''
        make test
      '';
      #doCheck = true;
    };

    json = stdenv.mkDerivation rec {
      name = "json-${version}";
      version = "2.0.4";
      src = fetchgit {
        url = "https://github.com/jihuni/json.git";
        rev = "62a670abbf1f9fe56fdfd02b9ae44c05b3382149";
        sha256 = "11qsnamijjiy7w1xfzka8scy1cymxpargvpprf7fmyj881s5and5";
      };
      buildInputs = [ cmake ];
      cmakeFlags = [ ];
      patches = [ ];
      enableParallelBuilding = true;
      #checkPhase = ''
      #  make test
      #'';
      installPhase = ''
        make install
        mkdir -p $out/include/json
        cp -a ../src/* $out/include/json
      '';
    };

    csv = stdenv.mkDerivation rec {
      name = "csv-${version}";
      version = "9bf299c";
      src = fetchgit {
        url = "https://github.com/ben-strasser/fast-cpp-csv-parser.git";
        rev = "9bf299cefb60d0e4ba7dc79d51491595ab1b3213";
        sha256 = "1xgmw46d3f1wbl6bmcz53ph1mrgzk1mrjnkvz4ypavzwg2q1vxnq";
      };
      installPhase = ''
        mkdir -p $out/include/csv
        cp -a *.h $out/include/csv
      '';
    };
    backwardcpp = stdenv.mkDerivation rec {
      name = "backward-cpp-${version}";
      version = "d820636";
      src = fetchgit {
        url = "https://github.com/bombela/backward-cpp.git";
        rev = "d820636e5af8f4114ce7839f740ff061f719d4db";
        sha256 = "0wj115m38sl3axhypy18v50693r9j7i29lhy1sw9d8qb522i74gq";
      };
      installPhase = ''
        mkdir -p $out/include/
        cp -a *.hpp $out/include/
      '';
    };
    xxhashct = stdenv.mkDerivation rec {
      name = "xxhashct";
      version = "1.0";
      src = fetchgit {
        url = "https://github.com/ekpyron/xxhashct";
        rev = "9da67392c38a3c5d92bcc4ef7074f0e02fe54a99";
        sha256 = "0z8irykzd8rsd6h88p30ffjay9k4jszk6w5sxxyky7p8fj3qrdb6";
      };
      #buildInputs = [ cmake ];
      installPhase = ''
        mkdir -p $out/include/xxhashct
        cp -a *.hpp $out/include/xxhashct
      '';
    };
    variant = stdenv.mkDerivation rec {
      name = "variant";
      version = "1.1.4";
      src = fetchgit {
        url = "https://github.com/mapbox/variant";
        rev = "d2588a8f1d6b5d480d228e6d8a906ce634bdea9a";
        sha256 = "1k103fqk9hk2l8bp27cy1bkgq7blmxdidsb6b5pdqd0xfy7xf52h";
      };
      dontBuild = true;
      #buildPhase = ''
      #'';
      installPhase   = ''
        mkdir -p $out
        cp -a include/ $out/
      '';


    };






    libsvm = stdenv.mkDerivation rec {
      name = "libsvm-${version}";
      version = "3.21";
      src = fetchgit {
        url = "https://github.com/cjlin1/libsvm.git";
        rev = "2fdc614c1526970b4412ba1db0cfcf772023ab61";
        sha256 = "0fp43iwi37iyp2fx7crs8l9xc21rbv7znn31mhjss8kmyv9nr0js";
      };

      buildPhase = ''
      make
      make lib
      '';
      installPhase = ''
      mkdir -p $out/libsvm
      cp -a * $out/libsvm
      '';

    };

    liblinear = stdenv.mkDerivation rec {
      name = "liblinear-${version}";
      version = "2.1";
      src = fetchgit {
        url = "https://github.com/cjlin1/liblinear.git";
        rev = "993963e43c6162c1d016ce77e7205ac24c1548de";
        sha256 = "1kv4nzzjd0qh7cik1ilry87ypg4dxlcqq5c28ckga3j7l0ky98l7";
      };

      buildPhase = ''
      make
      make lib
      '';
      installPhase = ''
      mkdir -p $out/liblinear
      cp -a * $out/liblinear
      cp -a liblinear.so.3 $out/liblinear/liblinear.so
      '';

    };
    
      
}
