{ pkgs, buildPythonPackage }:

with pkgs;

rec { 
    mirror_url="https://mirror.picosecond.org/pypi/packages";

    bz2file = buildPythonPackage rec {
      name = "bz2file-0.98";
      src = pkgs.fetchurl {
        url = "${mirror_url}/source/b/bz2file/${name}.tar.gz";
	sha256 = "126s53fkpx04f33a829yqqk8fj4png3qwg4m66cvlmhmwc8zihb4";
      };
      propagatedBuildInputs = [ ];
      meta = {
      };
    };

   smart_open = buildPythonPackage rec {
     name = "smart_open-1.2.1";
     src = pkgs.fetchurl {
       url = "${mirror_url}/source/s/smart_open/${name}.tar.gz";
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
        url = "${mirror_url}/source/u/untangle/${name}.tar.gz";
        sha256 = "0s0dv78xchq5z5hn9lja56y3wphrv3qk4c847cghf9nl1kz5lsni";
      };
      propagatedBuildInputs = [ ];
      meta = {
      };
    };
    Theano = buildPythonPackage rec {
      name = "Theano-0.8.2";
      src = pkgs.fetchurl {
        url = "${mirror_url}/30/3d/2354fac96ca9594b755ec22d91133522a7db0caa0877165a522337d0ed73/${name}.tar.gz";
        sha256 = "0c49mz3bg57vigkyfz3yd6302587hsikhvgkh7w7ny0sxpvwhqvl";
      };
      propagatedBuildInputs = [python27Packages.nose
                               python27Packages.numpy 
                               python27Packages.numpy.blas 
                               #python27Packages.pydot_ng 
                               python27Packages.scipy 
                               python27Packages.six ];
      meta = {
      };
    };
    Keras = buildPythonPackage rec {
      name = "Keras-1.0.3";
      src = pkgs.fetchurl {
        url = "${mirror_url}/d3/a6/ebf18e64af07e9d7a4e8aec14fe132d65eb31c66b49ef8311a61dbacef75/${name}.tar.gz";
        sha256 = "0wi826bvifvy12w490ghj1g45z5xb83q2cadqh425sg56p98khaq";
      };
      propagatedBuildInputs = [ Theano 
                                python27Packages.six 
                                python27Packages.pyyaml ];
      meta = {
      };
    };



   gensim = buildPythonPackage rec {
     name = "gensim-0.12.4";
     src = pkgs.fetchurl {
       url = "${mirror_url}/source/g/gensim/${name}.tar.gz";
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
