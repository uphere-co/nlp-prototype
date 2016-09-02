{ nixpkgs }:

let pkgs = import nixpkgs {};

in
{
  hello = pkgs.hello;

  which = pkgs.which;

}
