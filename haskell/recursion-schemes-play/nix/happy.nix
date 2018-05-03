{ mkDerivation, array, base, containers, mtl, process, stdenv }:
mkDerivation {
  pname = "happy";
  version = "1.19.4";
  sha256 = "6be499f66c61f8c48cbbbcb70515eb8e62c2bfa08adcc8c9474e7ae343a6936d";
  revision = "1";
  editedCabalFile = "09586bhhl6v0k36ga5zg353910287zjl4pxwa8h8v2lagsskjjrr";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ array base containers mtl ];
  testHaskellDepends = [ base process ];
  homepage = "http://www.haskell.org/happy/";
  description = "Happy is a parser generator for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
