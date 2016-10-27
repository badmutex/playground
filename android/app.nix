{release ? false
, platformVersion
}:

{
  name = "MyFirstApp-${if release then "release" else "debug"}";
  src = src/myfirstapp;
  
  antFlags = "-Dtarget=android-${platformVersion}";

  # keyStore = ./keystore;
  # keyAlias = "myfirstapp";
  # keyStorePassword = "mykeystore";
  # keyAliasPassword = "myfirstapp";
}
