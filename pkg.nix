{ mkDerivation, base, containers, monad-loops, stdenv }:
mkDerivation {
  pname = "onajiyou";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers monad-loops ];
  homepage = "https://github.com/laMudri/onajiyou#readme";
  description = "Kanji search tool";
  license = stdenv.lib.licenses.agpl3;
}
