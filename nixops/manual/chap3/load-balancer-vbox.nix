let

  vbox = { deployment.targetEnv = "virtualbox";
           deployment.virtualbox.memorySize = 1024; # megabytes
           deployment.virtualbox.headless = true;
         };

in

{ proxy = vbox;
  backend1 = vbox;
  backend2 = vbox;
}
