{ nixpkgs }:

let pkgs = import nixpkgs {};

in
{
  symbolic = import ../symbolic { inherit pkgs; };
  "rnn++"  = import ../rnn++ { inherit pkgs; };
}
