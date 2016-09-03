{ nixpkgs }:

let pkgs = import nixpkgs {};

in
{
  "symbolic" = import ../symbolic { inherit pkgs; };
  "rnn++"    = import ../rnn++    { inherit pkgs; };
  "word2vec" = import ../word2vec { inherit pkgs; };
}
