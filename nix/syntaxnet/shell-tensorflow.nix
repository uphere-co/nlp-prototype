{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let tensorflow-tool = callPackage ./default-tensorflow.nix { };
in
stdenv.mkDerivation {
  name = "tensorflow-env";
  version = "0.99";
  buildInputs =
  [
    which clang swig protobuf
    tensorflow-tool.bazel
  ]
  ++
  (with python27Packages;
  [
    numpy scipy mock
  ]);

}