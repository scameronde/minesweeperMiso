{ mkDerivation, base, miso, mtl, containers, random, MonadRandom, stdenv }:
mkDerivation {
  pname = "minesweeper";
  version = "0.1.0.0";
  src = src/.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso mtl containers random MonadRandom];
  description = "Miso Minesweeper";
  license = stdenv.lib.licenses.bsd3;
}
