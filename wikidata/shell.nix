{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
    removeDepends = pnames: depends:
      builtins.filter (e: !(builtins.elem (e.pname or "") pnames)) depends;


    hsconfig = import ../nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
    hsconfig2 = self: super: {
      "conduit" = haskell.lib.overrideCabal super.conduit (drv: {
        version = "1.2.9";
        sha256 = "01ng2k944hc258q3an4s1a6ym7g9j8fcr0jcdkwapdyw2s4rvpwa";
        libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.split ];
      });
      "distributed-process-async" = self.callPackage
        ({ mkDerivation, ansi-terminal, base, binary, containers
         , data-accessor, deepseq, distributed-process
         , distributed-process-tests, fingertree
         , hashable, HUnit, mtl, network, network-transport
         , network-transport-tcp, rematch, stm, test-framework
         , test-framework-hunit, time, transformers, unordered-containers
         }:
         mkDerivation {
           pname = "distributed-process-async";
           version = "0.2.3";
           src = fetchgit {
             url = "git://github.com/haskell-distributed/distributed-process-async.git";
             rev = "f571d3d4ced2f1d9caffdc08d80eade5cb0f061d";
             sha256 = "00jv144xns6diywbp35rgx94nlfqjd4dhz58svkwjhg1pislqvca";
           };
           
           sha256 = "d3031457c36bb3c35496031c185354417b54ce253e1878f35072d04e8161ad95";
           libraryHaskellDepends = [
             base binary containers data-accessor deepseq distributed-process
             fingertree hashable mtl stm time
             transformers unordered-containers
           ];
           testHaskellDepends = [
             ansi-terminal base binary deepseq distributed-process
             distributed-process-tests HUnit network
             network-transport network-transport-tcp rematch stm test-framework
             test-framework-hunit transformers
           ];
           homepage = "http://github.com/haskell-distributed/distributed-process-async";
           description = "Cloud Haskell Async API";
           license = stdenv.lib.licenses.bsd3;
           hydraPlatforms = stdenv.lib.platforms.none;
         }) {};

    };

    newHaskellPackages = haskellPackages.override {
      overrides = self: super: hsconfig self super // hsconfig2 self super;
    };

    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              aeson conduit conduit-extra distributed-process
              distributed-process-async
              #distributed-process-extras
              either monad-loops orc split text-format
            ]);
in stdenv.mkDerivation {
  name = "wikidata-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
     PS1="\n\[\033[0;34m\][\u@\h.wikidata:\w]\$\[\033[0m\] "  
  '';
}
