{ mkDerivation, aeson, attoparsec, base, base64-bytestring, binary
, bytestring, conduit, conduit-extra, connection, containers
, directory, distributed-process, filepath
, http-client, http-client-tls, http-types, monad-loops
, network-transport-zeromq, process, query-common
, scientific, stdenv, stm, text, transformers
, unix, unordered-containers, uuid, vector

, json, rnnpp, pqxx, tbb, fmt, hdf5_cpp, msgsl
}:
mkDerivation {
  pname = "query";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base base64-bytestring binary bytestring conduit
    conduit-extra connection containers directory distributed-process
    filepath http-client http-client-tls http-types monad-loops
    network-transport-zeromq process query-common scientific stm text
    transformers unix unordered-containers uuid vector
  ];
  executableSystemDepends = [
    hdf5_cpp rnnpp tbb json fmt pqxx msgsl
  ];
  homepage = "https://github.com/uphere-co/nlp-prototype";
  description = "querying result";
  license = "unknown";
}
