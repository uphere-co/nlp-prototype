{ stdenv, pkgs }:

with pkgs;

rec {
    clapack = stdenv.mkDerivation rec {
      name = "clapack";
      src = pkgs.fetchurl {
        url = "http://www.netlib.org/clapack/${name}.tgz";
	sha256 = "1b2z8mg1vi6l5d3dp9pd0v234hnjdb1srllgxnmcivjb2s1c7i3d";
      };

      buildNativeInputs = [ cmake ];

      configurePhase = ''
        cp make.inc.example make.inc
      '';
      buildPhase = ''
        make f2clib
	make blaslib
	make
      '';
      doCheck = false;
      checkPhase = "";
    };
    
    cblas = stdenv.mkDerivation rec {
      name = "cblas";
      src = pkgs.fetchurl {
        url = "http://www.netlib.org/blas/blast-forum/${name}.tgz";
        sha256 = "18x2c3n4zp913gsf89p1z9cdnqp9hkpx5kjpmydr1ggsczym8qqg";
      };
      propagatedBuildInputs = [ clapack ];
    };
    
    armadillo = stdenv.mkDerivation rec {
      name = "armadillo-7.200.2";
      src = pkgs.fetchurl {
        url = "http://sourceforge.net/projects/arma/files/${name}.tar.xz";
        sha256 = "1yvx75caks477jqwx5gspi6946jialddk00wdvg6dnh5wdi2xasm";
      };
      propagatedBuildInputs = [ cmake liblapack atlas blas cblas clapack ];
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
      configurePhase = ''
        ./configure --prefix=$out/mybin
      '';
      buildPhase = ''
      make
      '';
      meta = {
      
      };
    };
      
}