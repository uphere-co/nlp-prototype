{ mkDerivation, base, binary, bytestring, conduit, conduit-extra
, stdenv, text, unix
}:
mkDerivation {
  pname = "query-common";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring conduit conduit-extra text unix
  ];
  homepage = "https://github.com/uphere-co/nlp-prototype";
  description = "querying result";
  license = "unknown";
}
