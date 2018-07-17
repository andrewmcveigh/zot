let
  pkgs = import <nixpkgs> { };
  pkg = pkgs.haskellPackages.developPackage {
    root = ./.;
  };
  # hie-pkgs = import ./../hie-nix/default.nix { };
in
  pkg.overrideAttrs(attr: {
    buildInputs = [
      pkgs.ghc
      pkgs.cabal-install
      pkgs.stack
      pkgs.hlint
      # hie-pkgs.hie82
    ];
  })
