with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskell-ng.packages.ghcjs.override {
      overrides = self: super: {
        francium = self.callPackage ../. {};
        todo-mvc = self.callPackage ./. {};
        virtual-dom = self.callPackage ../../virtual-dom {};
      };
    };
in modifiedHaskellPackages.todo-mvc.env
