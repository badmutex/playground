{
  pkgs ? import <nixpkgs> {}
}:

pkgs.androidenv.androidsdk {
  platformVersions = [ "23" ];
  abiVersions = [ "armeabi-v7a" "x86_64_23" ];
  useGoogleAPIs = true;
  useExtraSupportLibs = true;
  useGooglePlayServices = true;
}
