{ mkDerivation, array, attoparsec, base, bifunctors, bytestring
, conduit, conduit-extra, containers, hashable, lbfgs, llvm-general
, llvm-general-pure, MemoTrie, resourcet, stdenv, symbolic, text
, transformers, unordered-containers, vector, vector-algorithms
}:
mkDerivation {
  pname = "autoencode";
  version = "0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bifunctors bytestring conduit conduit-extra
    hashable llvm-general-pure MemoTrie resourcet symbolic text
    transformers unordered-containers vector
  ];
  executableHaskellDepends = [
    array attoparsec base bifunctors bytestring containers hashable
    lbfgs llvm-general MemoTrie symbolic text transformers
    unordered-containers vector vector-algorithms
  ];
  homepage = "https://github.com/uphere-co";
  description = "Recursive Autoencoder for Paraphrase detection";
  license = stdenv.lib.licenses.bsd3;
}
