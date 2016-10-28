let pkgs = import <nixpkgs> {};
in
with pkgs.haskellPackages;
let
  iterable = callPackage ./nix/iterable.nix {};
  Octree = callPackage ./nix/Octree.nix {};
  hPDB = callPackage ./nix/hPDB.nix { inherit iterable Octree; };
  kdt = callPackage ./nix/kdt.nix {};
  # pretty = callPackage ./nix/pretty.nix {};
  # prettyNcols = callPackage ./nix/prettyNcols.nix {inherit pretty;};
in pkgs.myEnvFun {
  name = "Haskell";
  buildInputs = [
    ghc
    hPDB
    kdt
    # pretty
    # prettyNcols
    hmatrix
    primitive
    ListLike
  ];
}