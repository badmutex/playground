let
  pkgs = import <nixpkgs> { config = import ./config.nix; };
in
  pkgs.haskellPackages.recursion-schemes-play.env
