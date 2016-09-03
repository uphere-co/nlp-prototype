{ nixpkgs }:

let pkgs = import nixpkgs {};

in
{
  symbolic = import ../symbolic/default.nix {};
  #hello = pkgs.hello;
}
