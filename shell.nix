{ pkgs ? import <nixpkgs> {} }:

let
  drv = pkgs.haskellPackages.callPackage ./. {};

  tools = with pkgs.haskellPackages; [
    cabal-install
    hlint
    apply-refact
    hindent
    hasktags
    hoogle
    cabal2nix
  ];

in
  pkgs.stdenv.mkDerivation {
    name = "devEnv";
    buildInputs = tools ++ drv.env.buildInputs;
    nativeBuildInputs = drv.env.nativeBuildInputs;
  }


