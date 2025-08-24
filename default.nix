{
  pkgs ? import <nixpkgs> { },
}:
pkgs.haskellPackages.developPackage {
  root = ./.;

  modifier =
    drv:
    drv.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.zlib ];
    });
}
