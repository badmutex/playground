{ nixpkgs ? <nixpkgs>
, systems ? [ "x86_64-linux" ]
, buildPlatformVersions ? [ "23" ]
, emulatePlatformVersions ? [ "23" ]
, abiVersions ? [ "armeabi-v7a" ]
}:


with builtins;

let

  pkgs = import nixpkgs;

  call = scopeset: function: paramset: function (scopeset // paramset);

  build = { release
          , platformVersion
          , appdefpath
          }:
          let appdef = pkgs.callPackage ./app.nix {};
          in
          pkgs.androidenv.buildApp
          { platformVersions = [ platformVersion ];
            inherit release;
          } // appdef;

  app2 = { release ? false
         , system, platformVersion
         }:
           { name = "api_" + platformVersion +  "-host_" + system;
             value = build { inherit release platformVersion; };
           };


  combinations =
    { release ? false}:
    let
      l = map mk attrs;
      mk = attr: let b = call attr app2 {}; # app2 { inherit (attr) release platformVersion system; };
                 in listToAttrs [
                      { name = b.name;
                        value = b.value;
                      }
                    ];
      attrs = map (system: map (version: listOfAttrs [ {name = "system"; value = system;}
                                                       {name = "platformVersion"; value = version;}
                                                       {name = "release"; value = release;}
                                                     ])
                               buildPlatformVersions)
                  systems;
    in l;
  
  app = 
    { release ? false }: listToAttrs (map (system:
      let pkgs = import nixpkgs { inherit system; };
      in
      { name = "host_" + system;
        value = listToAttrs (map (buildPlatformVersion:
          { name = "build_" + buildPlatformVersion;
            value = pkgs.androidenv.buildApp ( import ./app.nix {
              inherit release;
              platformVersion = buildPlatformVersion;
            });
          }
        ) buildPlatformVersions);
      }
    ) systems);

in

rec {
  debug = combinations {};
  release = combinations {release = true;};
}
