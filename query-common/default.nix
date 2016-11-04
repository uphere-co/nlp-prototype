{ mkDerivation, base, binary, bytestring, conduit, conduit-extra
, stdenv, text, unix, hashable
}:
mkDerivation {
  pname = "query-common";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring conduit conduit-extra text unix hashable
  ];
  homepage = "https://github.com/uphere-co/nlp-prototype";
  description = "querying result";
  license = "unknown";
}
