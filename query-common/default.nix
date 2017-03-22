{ mkDerivation, base, aeson, binary, bytestring, conduit, conduit-extra
, distributed-process, unordered-containers, monad-loops
, stdenv, text, unix, hashable, network-simple, stm
}:
mkDerivation {
  pname = "query-common";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base aeson binary bytestring conduit conduit-extra
    distributed-process unordered-containers monad-loops
    text unix hashable network-simple stm
  ];
  homepage = "https://github.com/uphere-co/nlp-prototype";
  description = "querying result";
  license = "unknown";
}
