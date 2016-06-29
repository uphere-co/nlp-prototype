{ pkgs, buildPythonPackage }:

with pkgs;

rec { 
    mirror_url="https://pypi.python.org/simple";

    bz2file = buildPythonPackage rec {
      name = "bz2file-0.98";
      src = pkgs.fetchurl {
        url = "${mirror_url}/bz2file/${name}.tar.gz";
	sha256 = "126s53fkpx04f33a829yqqk8fj4png3qwg4m66cvlmhmwc8zihb4";
      };
      propagatedBuildInputs = [ ];
      meta = {
      };
    };

   smart_open = buildPythonPackage rec {
     name = "smart_open-1.2.1";
     src = pkgs.fetchurl {
       url = "${mirror_url}/smart_open/${name}.tar.gz";
       sha256 = "08nczpmkwpnfgnvag791gql35mbwk0xyar6p0cvb0lawvrs561wv";
     };
     buildInputs = [];
     propagatedBuildInputs = [ python27Packages.boto bz2file ];
     doCheck = false;
     meta = {
     };
   };

    untangle = buildPythonPackage rec {
      name = "untangle-1.1.0";
      src = pkgs.fetchurl {
        url = "${mirror_url}/untangle/${name}.tar.gz";
        sha256 = "0s0dv78xchq5z5hn9lja56y3wphrv3qk4c847cghf9nl1kz5lsni";
      };
      propagatedBuildInputs = [ ];
      meta = {
      };
    };

   gensim = buildPythonPackage rec {
     name = "gensim-0.12.4";
     src = pkgs.fetchurl {
       url = "${mirror_url}/gensim/${name}.tar.gz";
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
    pytest-mock = buildPythonPackage rec {
     name = "pytest-mock-1.1";
     src = pkgs.fetchurl {
       url = "https://pypi.python.org/packages/99/0e/45906c1e876b175cb51d8710075be900948f44a5f6a92c90095bdcd846c8/${name}.zip";
       sha256 = "0gmlh1jzcs385d0764gshmmyszid70v8sc185pmz7gb97idza461";
     };
     buildInputs = [];
     propagatedBuildInputs = [python27Packages.pytest
                              python27Packages.mock];
     meta = {
     };
    };
    guppy = buildPythonPackage rec {
     name = "guppy-0.1.10";
     src = pkgs.fetchurl {
       #url = "${mirror_url}/guppy/${name}.tar.gz";
       url = "https://pypi.python.org/packages/2b/b0/8614c981bc40c10ec4e24a7428e998d05d11e4b9680c718952207ff63a26/${name}.tar.gz";
       sha256 = "1pqk730fbp47q99la0f08h69c41dcd93nsmv2vm664251q08q480";
     };
     buildInputs = [];
     propagatedBuildInputs = [];
     meta = {
     };
   };
   nltk = buildPythonPackage rec {
     name = "nltk-3.2.1";
     src = pkgs.fetchurl {
       #url = "${mirror_url}/nltk/${name}.tar.gz";
       url = "https://pypi.python.org/packages/58/85/8fa6f8c488507aab7d6234ce754bbbe61bfeb8382489785e2d764bf8f52a/${name}.tar.gz";
       sha256 = "0skxbhnymwlspjkzga0f7x1hg3y50fwpfghs8g8k7fh6f4nknlym";
     };
     buildInputs = [];
     propagatedBuildInputs = [];
     meta = {
     };
   };
}
