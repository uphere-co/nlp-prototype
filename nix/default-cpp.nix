{ stdenv, pkgs }:

with pkgs;

rec {
    armadillo = stdenv.mkDerivation rec {
      name = "armadillo-7.200.2";
      src = pkgs.fetchurl {
        url = "http://sourceforge.net/projects/arma/files/${name}.tar.xz";
        sha256 = "1yvx75caks477jqwx5gspi6946jialddk00wdvg6dnh5wdi2xasm";
      };
      buildInputs = [ cmake ]; 
      propagatedBuildInputs = [ openblas ]; 
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
      
}