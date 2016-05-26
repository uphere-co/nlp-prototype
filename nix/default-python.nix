{ pkgs, buildPythonPackage }:

with pkgs;

rec { 

   gensim = buildPythonPackage rec {
     name = "gensim-0.12.4";
     src = pkgs.fetchurl {
       url = "https://pypi.python.org/packages/source/g/gensim/${name}.tar.gz";
       sha256 = "05daz1hhhx8adqrmykcmd26vdfif2zw4c2gazz0zsa9vs5ngminp";
     };
     buildInputs = [];
     propagatedBuildInputs = [];
     meta = {
     };
   };
}
