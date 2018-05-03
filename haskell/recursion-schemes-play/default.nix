{ mkDerivation, base, recursion-schemes, stdenv }:
mkDerivation {
  pname = "recursion-schemes-play";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base recursion-schemes ];
  license = stdenv.lib.licenses.bsd3;
}
