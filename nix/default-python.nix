{ pkgs, buildPythonPackage }:

with pkgs;

rec { 
    bz2file = buildPythonPackage rec {
      name = "bz2file-0.98";
      src = pkgs.fetchurl {
        url = "https://pypi.python.org/packages/source/b/bz2file/${name}.tar.gz";
	sha256 = "126s53fkpx04f33a829yqqk8fj4png3qwg4m66cvlmhmwc8zihb4";
      };
      propagatedBuildInputs = [ ];
      meta = {
      };
    };

   smart_open = buildPythonPackage rec {
     name = "smart_open-1.2.1";
     src = pkgs.fetchurl {
       url = "https://pypi.python.org/packages/source/s/smart_open/${name}.tar.gz";
       sha256 = "08nczpmkwpnfgnvag791gql35mbwk0xyar6p0cvb0lawvrs561wv";
     };
     buildInputs = [];
     propagatedBuildInputs = [ python27Packages.boto bz2file ];
     doCheck = false;
     meta = {
     };
   };


   gensim = buildPythonPackage rec {
     name = "gensim-0.12.4";
     src = pkgs.fetchurl {
       url = "https://pypi.python.org/packages/source/g/gensim/${name}.tar.gz";
       sha256 = "05daz1hhhx8adqrmykcmd26vdfif2zw4c2gazz0zsa9vs5ngminp";
     };
     buildInputs = [];
     propagatedBuildInputs = [ smart_open
                               python27Packages.six
			       python27Packages.cython
                               python27Packages.scipy
			       python27Packages.numpy
                             ];
     meta = {
     };
   };
}
