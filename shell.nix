{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "eb64583e3e15749b3ae56573b2aebbaa9cbab4eb";
      sha256 = "sha256-KgW8EStkiIEOOU+fWyNHDgYZEs5t5FBtYa9tdWbVJ0M=";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_14_3
    easy-ps.spago
    easy-ps.purs-tidy
  ];
}