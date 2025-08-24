{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = with pkgs; [
    cabal-install
    ghc
    haskell-language-server
  ];

  nativeBuildInputs = with pkgs; [
    zlib
  ];
}
