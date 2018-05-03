{ packageOverrides = pkgs: {
    haskellPackages = pkgs.haskell.packages.ghc842.override {
      overrides = hsPkgsNew: hsPkgsOld: {
        recursion-schemes-play = hsPkgsNew.callPackage ./default.nix {};
        # language-python = hsPkgsNew.callPackage ./nix/language-python.nix {};
        # happy = pkgs.haskell.lib.dontCheck (hsPkgsNew.callPackage ./nix/happy.nix {});
        # alex = pkgs.haskell.lib.dontCheck hsPkgsOld.alex;
      };
    };
  };
}

