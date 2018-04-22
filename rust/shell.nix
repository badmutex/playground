{ pkgs ? import <nixpkgs> {}
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

    cargo
    rustfmt
    rustc
    rust-bindgen
  ]);
  runScript = "bash";
}).env
