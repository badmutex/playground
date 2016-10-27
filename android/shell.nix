{ pkgs ? import <nixpkgs> {} }:
 
let

  sdk = pkgs.callPackage ./sdk.nix {};

in pkgs.stdenv.mkDerivation {
  name = "android-env-shell";
  nativeBuildInputs = [ sdk ];
  shellHook = ''
    export USE_CCACHE=1
    export ANDROID_HOME=${sdk}
  '';
}
