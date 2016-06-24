{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let toolz = callPackage ./default-cpp.nix {
              pkgs=pkgs;
            };
in
stdenv.mkDerivation {
  name = "cpp-env";
  buildInputs = [ gcc boost blas liblapack
  	      	   toolz.armadillo toolz.libpca
                ];
  shellHook = ''
  export CPATH=${toolz.libpca}/mybin/include:${toolz.armadillo}/include/armadillo_bits:$CPATH
  export LIBRARY_PATH=${toolz.libpca}/mybin/lib:${toolz.armadillo}/lib:$LIBRARY_PATH
  export LD_LIBRARY_PATH=${toolz.libpca}/mybin/lib:${toolz.armadillo}/lib:$LD_LIBRARY_PATH
  export LD_RUN_PATH=${toolz.libpca}/mybin/lib:${toolz.armadillo}/lib:$LD_RUN_PATH
  '';
}