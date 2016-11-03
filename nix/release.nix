{ nixpkgs }:

let pkgs = import nixpkgs {};
    toolz_cpp = pkgs.callPackage ../nix/default-cpp.nix {};
in
rec {
  "autoencode"   = import ../autoencode/release.nix { inherit pkgs; };
  "symbolic"     = import ../symbolic/release.nix { inherit pkgs; };
  "query-common" = import ../query-common/release.nix {
                   }; 
  "query"        = import ../query/release.nix {
                     inherit pkgs;
                     hdf5_cpp = pkgs.hdf5-cpp;
                     tbb = pkgs.tbb;
                     inherit rnnpp;
                     json = toolz_cpp.json;
		     fmt = toolz_cpp.fmt;
		     libpqxx =pkgs.libpqxx;
                   };
  
  "rnnpp"        = import ../rnn++    { inherit pkgs; };
  "word2vec"     = import ../word2vec { inherit pkgs; };
}
