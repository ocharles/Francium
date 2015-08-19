with (import <nixpkgs> {}).pkgs;
let 
  try-reflex = import ./try-reflex.nix {};
  modifiedHaskellPackages = try-reflex.ghcjs.override {
    overrides = self: super: {
      francium = self.callPackage ../rb-2 {};
      todo-mvc = self.callPackage ./. {};
      virtual-dom = self.callPackage ../../virtual-dom {};
      frpnow = self.callPackage ../../frpnow {};
      reactive-banana = self.callPackage ../../reactive-banana/reactive-banana {};
    };
  };
in modifiedHaskellPackages.todo-mvc.env
