{ nixpkgs }:

let pkgs = import nixpkgs {};

in
{
  "autoencode" = import ../autoencode/release.nix { inherit pkgs; };
  "symbolic"   = import ../symbolic/release.nix { inherit pkgs; };
  "rnn++"      = import ../rnn++    { inherit pkgs; };
  "word2vec"   = import ../word2vec { inherit pkgs; };
}
