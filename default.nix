let
  pkgs = import <nixpkgs>{ };
  stdenv = pkgs.stdenv;
in stdenv.mkDerivation rec {
  name = "zot";
  buildInputs = [
    pkgs.ghc
    pkgs.cabal-install
  ];
}
