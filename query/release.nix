{ pkgs ? (import <nixpkgs>{})
, hdf5_cpp
, tbb
, rnnpp
, msgsl
, json
, fmt
}:

with pkgs;

let 
    hsconfig1 = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    hsconfig2 = self: super: { 
      query = self.callPackage (import ./default.nix) {
        inherit hdf5_cpp tbb rnnpp msgsl json fmt;
      };
    };
    hsconfig = self: super: (hsconfig1 self super // hsconfig2 self super); 
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };
    
in newhaskellPackages.query