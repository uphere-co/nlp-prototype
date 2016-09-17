{ pkgs ? import <nixpkgs> {}
, rnnpp ? import ../rnn++ {}
}:

with pkgs;

let toolz_cpp = callPackage ../nix/default-cpp.nix {};
    hsenv = haskellPackages.ghcWithPackages (p: with p; [ ]);
in stdenv.mkDerivation {
  name = "query-dev";
  buildInputs = [ hsenv hdf5 hdf5-cpp liblbfgs cppzmq zeromq
		  rnnpp tbb
                  toolz_cpp.msgsl
		  toolz_cpp.json 
                ];
  shellHook = ''
  '';
}
