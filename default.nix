{ mkDerivation, attoparsec, base, binary, bytestring
, classy-prelude, containers, directory, errors, filepath, flow
, lens, lens-aeson, mmorph, mtl, pipes, pipes-bytestring
, pipes-parse, process, protolude, stdenv, text, transformers
, uri-bytestring, yaml
}:
mkDerivation {
  pname = "play-in-mpv";
  version = "1.4.3";
  src = ./launcher;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base classy-prelude errors flow lens mmorph protolude
  ];
  executableHaskellDepends = [
    attoparsec base binary bytestring containers directory filepath
    lens-aeson mtl pipes pipes-bytestring pipes-parse process text
    transformers uri-bytestring yaml
  ];
  homepage = "https://github.com/Szczyp/play-in-mpv#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.gpl3;
}
