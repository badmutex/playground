{
 nixpkgs ? import <nixpkgs> {},
 compiler ? "ghc7103"
}:

let

  inherit (nixpkgs) pkgs;

  ghc = pkgs.haskell.packages.${compiler}.ghcWithHoogle (ps: with ps; [
    reflection
    singletons
  ]);

in

pkgs.stdenv.mkDerivation {
  name = "playground-haskell-dependent_types";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
