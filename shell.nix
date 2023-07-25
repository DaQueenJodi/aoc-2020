{ pkgs ? import <nixpkgs> {} }: with pkgs;
mkShell {
  packages = [
    ghc
    haskell-language-server
  ];
}
