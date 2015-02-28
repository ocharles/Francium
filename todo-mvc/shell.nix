with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskell-ng.packages.ghcjs.override {
      overrides = self: super: {
        francium = self.callPackage ../. {};
        todo-mvc = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.todo-mvc.env
