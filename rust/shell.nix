
let
  pkgs_base = import <nixpkgs> {};
  nixpkgs-mozilla = pkgs_base.fetchgit {
    url = "git://github.com/mozilla/nixpkgs-mozilla.git";
  	sha256 = "1r14c21x7x2h3v8gmng1g8g6n0c7hr46s5p60plqfh18sf2kp845";
  };
  rust-overlay = import "${nixpkgs-mozilla}/rust-overlay.nix";

in

{ pkgs ? import <nixpkgs> { overlays = [ rust-overlay ]; }
, ...
}:


with pkgs;

(buildFHSUserEnv {
  name = "rust-playground-devenv";
  targetPkgs = pkgs: (with pkgs; [
    binutils
    clang
    libtool
    file
    gnumake
    which
    openssl.dev
    jq
    pkgconfig
  ] ++ (with latest.rustChannels.stable; [
    cargo
    rustfmt
    rust
    rust-bindgen
  ]));
  runScript = "bash";
}).env
