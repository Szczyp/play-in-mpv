{ pkgs ? import ./nixpkgs.nix }:
with pkgs;
let
  name = "play-in-mpv";

  src = nix-gitignore.gitignoreSource [] ./.;

  haskellPackages = haskell.packages.ghc865.override {
    overrides = self: super: {
      ${name} = self.callCabal2nix name src {};
    };
  };

  drv = haskell.lib.justStaticExecutables haskellPackages.${name};

  ghcide = (import (fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc865;

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
      ghcide
    ];
  };
in
drv // { inherit shell; }
