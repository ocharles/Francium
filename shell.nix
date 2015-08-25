with (import <nixpkgs> {}).pkgs;
let overrideCabal = drv: f: if drv == null then null else (drv.override (args:
args // {
        mkDerivation = drv: args.mkDerivation (drv // f drv);
            })) // {
                    overrideScope = scope: overrideCabal (drv.overrideScope
                    scope) f;
                        };
    modifiedHaskellPackages = haskell-ng.packages.ghcjs.override {
      overrides = self: super: {
        francium = self.callPackage ./. {};
        frpnow = self.callPackage ../../frpnow {};
        reactive-banana = self.callPackage ../../reactive-banana/reactive-banana {};
        ghcjs-base = overrideCabal super.ghcjs-base (drv: { 
          src = /home/ollie/work/ghcjs/ghcjs-base; 
        });
        ghcjs-dom = overrideCabal super.ghcjs-dom (drv: {
          version = "0.2.1.0";
          src = fetchgit {
            url = git://github.com/ghcjs/ghcjs-dom;
            rev = "d6eb927ae279071495f5edd7413bef517508bc7d";
            sha256 = "c5cc066fd16a7838b6cb51d151d8d01264ac682228fd1730a9f08cf3437c6f3c";
          };
        });
      };
    };
in
  modifiedHaskellPackages.francium.env
