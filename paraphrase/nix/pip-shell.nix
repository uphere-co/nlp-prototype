{ pkgs ? (import <nixpkgs> {}), pythonPackages ? "python27Packages" }:

with pkgs.lib;

let
  basePythonPackages = with builtins; if isAttrs pythonPackages
    then pythonPackages
    else getAttr pythonPackages pkgs;

  pythonPackagesWithLocals = basePythonPackages.override (a: {
    self = pythonPackagesWithLocals;
  })
  // (scopedImport {
    self = pythonPackagesWithLocals;
    super = basePythonPackages;
    inherit pkgs;
    inherit (pkgs) fetchurl fetchgit;
  } ./python-packages.nix)
  // { pip = basePythonPackages.pip; };

  myPythonPackages =
    pythonPackagesWithLocals;

in pkgs.stdenv.mkDerivation {
  name = "pip-shell";
  buildInputs = [ myPythonPackages.bllipparser myPythonPackages.six ];
  shellHook = ''
  '';
}
