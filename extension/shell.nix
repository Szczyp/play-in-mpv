with import ./nixpkgs.nix;
mkShell {
  buildInputs = with nodePackages; [ web-ext typescript typescript-language-server ];
}
