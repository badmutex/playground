{androidenv ? (with (import <nixpkgs> {}).androidenv)}:

androidenv.buildApp {
  name = "MyFirstApp";
  src = "src/myfirstapp";
  platformVersions = [ "23" ];
  useGoogleAPIs = true;

  release = true;
  keyStore = ./keystore;
  keyAlias = "badi";
  keyStorePassword = "foobar";
  keyAliasPassword = "foobar";
}
