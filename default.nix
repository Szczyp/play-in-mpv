{ pkgs ? import <nixpkgs> {} }:
let
  name = "play-in-mpv";

  src = pkgs.nix-gitignore.gitignoreSource [] ./launcher;

  haskellPackages = pkgs.haskell.packages.ghc865.override {
    overrides = self: super: {
      ${name} = self.callCabal2nix name src {};
    };
  };

  drv = pkgs.haskell.lib.justStaticExecutables haskellPackages.${name};

  shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [ p.${name} ];
    buildInputs = with haskellPackages; [
      cabal-install
      apply-refact
      hindent
      hlint
      stylish-haskell
      hasktags
      hoogle
      (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc865
    ];
  };
in
drv // { inherit shell; }
