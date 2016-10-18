{ nixpkgs }:

let pkgs = import nixpkgs {};
    toolz_cpp = pkgs.callPackage ../nix/default-cpp.nix {};
in
rec {
  "autoencode" = import ../autoencode/release.nix { inherit pkgs; };
  "symbolic"   = import ../symbolic/release.nix { inherit pkgs; };
  "query"      = import ../query/release.nix {
                   inherit pkgs;
                   hdf5_cpp = pkgs.hdf5-cpp;
                   tbb = pkgs.tbb;
                   msgsl = toolz_cpp.msgsl;
                   inherit rnnpp;
                   json = toolz_cpp.json;
		   fmt = toolz_cpp.fmt;
                 };
  
  "rnnpp"      = import ../rnn++    { inherit pkgs; };
  "word2vec"   = import ../word2vec { inherit pkgs; };
}
