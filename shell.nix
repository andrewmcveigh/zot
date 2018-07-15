let
  pkgs = import <nixpkgs> { };
  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;
  };
in
  pkg.overrideAttrs(attr: {
    buildInputs = [
      pkgs.ghc
      pkgs.cabal-install
      pkgs.stack
      pkgs.hlint
    ];
  })
