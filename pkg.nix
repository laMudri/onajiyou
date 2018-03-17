{ mkDerivation, base, brick, containers, stdenv }:
mkDerivation {
  pname = "onajiyou";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base brick containers ];
  homepage = "https://github.com/laMudri/onajiyou#readme";
  description = "Kanji search tool";
  license = stdenv.lib.licenses.agpl3;
}
