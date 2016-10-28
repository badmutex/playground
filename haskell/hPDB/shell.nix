let pkgs = import <nixpkgs> {};
in
with pkgs.haskellPackages;
let
  iterable = callPackage ./nix/iterable.nix {};
  Octree = callPackage ./nix/Octree.nix {};
  hPDB = callPackage ./nix/hPDB.nix { inherit iterable Octree; };
in pkgs.myEnvFun {
  name = "Haskell";
  buildInputs = [
    ghc
    hPDB
  ];
}