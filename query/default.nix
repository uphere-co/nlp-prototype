{ mkDerivation, aeson, base, binary, bytestring, conduit
, conduit-extra, directory, distributed-process, filepath, hdf5_cpp
, monad-loops, network-transport-zeromq,stdenv
, stm, tbb, text, unix, uuid
, rnnpp, msgsl, json, fmt, libpqxx
, base64-bytestring
, http-client
, http-client-tls
}:
mkDerivation {
  pname = "query";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring conduit conduit-extra unix http-client http-client-tls
  ];
  executableHaskellDepends = [
    aeson base binary bytestring conduit conduit-extra directory
    distributed-process filepath monad-loops network-transport-zeromq
    stm text unix uuid base64-bytestring
  ];
  executableSystemDepends = [ hdf5_cpp rnnpp tbb msgsl json fmt libpqxx ];
  homepage = "https://github.com/uphere-co/nlp-prototype";
  description = "querying result";
  license = "unknown";
}
