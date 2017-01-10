{ mkDerivation, base, aeson, binary, bytestring, conduit, conduit-extra
, stdenv, text, unix, hashable, network-simple
}:
mkDerivation {
  pname = "query-common";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base aeson binary bytestring conduit conduit-extra text unix hashable network-simple
  ];
  homepage = "https://github.com/uphere-co/nlp-prototype";
  description = "querying result";
  license = "unknown";
}
