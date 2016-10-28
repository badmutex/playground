let pkgs = import <nixpkgs> {};
in
with pkgs.haskellPackages;
let
  kdt = callPackage ./kdt.nix {};
in pkgs.myEnvFun {
  name = "Haskell";
  buildInputs = [
    ghc
    kdt
    random
  ];
}