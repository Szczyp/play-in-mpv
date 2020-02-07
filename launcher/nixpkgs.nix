let
  rev = "8f58eba2948239c2f3f10d3cbc0c161a6c3ffdd2";
  sha = "1vh9vykgm0qrdpv9dvzg36db3c44yr09cn0v0wxadcjyly79xz62";
in import (fetchTarball {
  url = "https://github.com/Szczyp/nixpkgs/archive/${rev}.tar.gz";
  sha256 = sha;
}) {}
