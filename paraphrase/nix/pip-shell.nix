{ pkgs ? (import <nixpkgs> {}), pythonPackages ? "python27Packages" }:

with pkgs.lib;

let
  basePythonPackages = with builtins; if isAttrs pythonPackages
    then pythonPackages
    else getAttr pythonPackages pkgs;

  myPythonPackages = basePythonPackages.override (a: {
    self = myPythonPackages;
  })
  // (scopedImport {
    self = myPythonPackages;
    super = basePythonPackages;
    inherit pkgs;
    inherit (pkgs) fetchurl fetchgit;
  } ./python-packages.nix)
  // { pip = basePythonPackages.pip; };

in pkgs.stdenv.mkDerivation {
  name = "pip-shell";
  buildInputs = [ myPythonPackages.bllipparser
                  myPythonPackages.six ];
  shellHook = ''
  '';
}
