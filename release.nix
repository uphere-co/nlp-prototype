{ nixpkgs
, uphere-nix-overlay
}:

let pkgs = import nixpkgs {};
    toolz_cpp = pkgs.callPackage (uphere-nix-overlay + "/nix/default-cpp.nix") {};
in
rec {
  "query-common" = import ./query-common/release.nix {
                     inherit uphere-nix-overlay;
                   }; 
  "query"        = import ./query/release.nix {
                     inherit pkgs uphere-nix-overlay;
                     hdf5_cpp    = pkgs.hdf5-cpp;
                     tbb         = pkgs.tbb;
                     inherit rnnpp;
                     json        = toolz_cpp.json;
		     fmt         = toolz_cpp.fmt;
		     msgsl       = toolz_cpp.msgsl;
                     xxhashct    = toolz_cpp.xxhashct;
                     flatbuffers = toolz_cpp.flatbuffers;
		     libpqxx     = pkgs.libpqxx;
		     elfutils    = pkgs.elfutils;
		     spdlog      = toolz_cpp.spdlog;
		     backwardcpp = toolz_cpp.backwardcpp;
                     variant     = toolz_cpp.variant;
                   };
  
  "rnnpp"        = import ./rnn++    { inherit pkgs uphere-nix-overlay; };
  "word2vec"     = import ./word2vec { inherit pkgs uphere-nix-overlay; };
}
