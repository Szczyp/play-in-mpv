{ pkgs ? import ./nixpkgs.nix }:
with pkgs;
let
  name = "play-in-mpv";

  src = nix-gitignore.gitignoreSource [] ./.;

  hp = haskellPackages.override {
    overrides = self: super: {
      ${name} = self.callCabal2nix name src {};
    };
  };

  ghcide = import (fetchTarball {
    url = "https://github.com/hercules-ci/ghcide-nix/archive/c940edd61eedba6750671fc142c9422cced73528.tar.gz";
    sha256 = "01f2x5sgncd468h99w3mpkkb1203akachm12czmiwbvgishf7dwp";
  }) {};

  drv = haskell.lib.justStaticExecutables hp.${name};

  shell = hp.shellFor {
    withHoogle = true;
    packages = p: [ p.${name} ];
    buildInputs = with hp; [
      cabal-install
      apply-refact
      hindent
      hlint
      stylish-haskell
      hasktags
      hoogle
      cabal2nix
      ghcide.ghcide-ghc865
    ];
  };
in
drv // { inherit shell; }
