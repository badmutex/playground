let pkgs = import <nixpkgs> {};
in
with pkgs.haskellPackages;
pkgs.myEnvFun {
  name = "HaskellDevEnv";
  buildInputs = [
    ghc
    cabalInstall
    hscolour
    random
    editDistance
    randomFu
  ];
}