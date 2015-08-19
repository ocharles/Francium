{ system ? null, config ? null }:
let overrideCabal = drv: f: if drv == null then null else (drv.override (args: args // {
      mkDerivation = drv: args.mkDerivation (drv // f drv);
    })) // {
      overrideScope = scope: overrideCabal (drv.overrideScope scope) f;
    };
    nixpkgs = import ./nixpkgs ({
      config.allowUnfree = true;
    } // (
      if system == null then {} else { inherit system; }
    ) // (
      if config == null then {} else { inherit config; }
    ));
    hspecGit = nixpkgs.fetchgit {
      url = git://github.com/ryantrinkle/hspec;
      rev = "937c0ae61d70dcd71c35a170b800c30f14a5bc9c";
      sha256 = "1819d5b3f973b432339256ba783b33ada691a785d059e83009e5e2edc6178f6d";
    };
    combineOverrides = old: new: (old // new) // {
      overrides = self: super:
        let oldOverrides = old.overrides self super;
        in oldOverrides // new.overrides self (super // oldOverrides);
    };
    makeRecursivelyOverridable = x: old: x.override old // {
      override = new: makeRecursivelyOverridable x (combineOverrides old new);
    };
    extendHaskellPackages = haskellPackages: makeRecursivelyOverridable haskellPackages {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {};
        reflex-dom = self.callPackage ./reflex-dom {};
        reflex-todomvc = self.callPackage ./reflex-todomvc {};
        these = overrideCabal super.these (drv: { 
          version = "0.5.0.0";
          src = nixpkgs.fetchgit {
            url = git://github.com/ryantrinkle/these;
            rev = "36e7dc3e55c85b2d501c7ddc5e77a9a6bb522db2";
            sha256 = "8841dd7426ad5e0edd05599a0896a6033043f8fa7faf6f7f4c6f88ef1d0209c7";
          };
          revision = null;
          editedCabalFile = null;
        });
        lens = overrideCabal super.lens (drv: {
          version = "4.12.3";
          sha256 = "0898z1ws9sy77yfhvx5did0pibpp81yxz0jg418gdx3znd39vyj8";
        });
        profunctors = overrideCabal super.profunctors (drv: {
          version = "5.1.1";
          sha256 = "0lw2ipacpnp9yqmi8zsp01pzpn5hwj8af3y0f3079mddrmw48gw7";
          revision = null;
          editedCabalFile = null;
        });
        bifunctors = overrideCabal super.bifunctors (drv: {
          version = "5";
          sha256 = "13990xdgx0n23qgi18ghhmsywj5zkr0a5bim0g8a4nzi0cx95ps1";
          buildDepends = with self; [
            semigroups
            tagged
          ];
        });
        reflection = overrideCabal super.reflection (drv: {
          version = "2";
          sha256 = "1hlrji6wyqr892a78px7wilhywyiqdfdmnr7zp4c641qks4rw6gf";
        });
        adjunctions = overrideCabal super.adjunctions (drv: {
          version = "4.2.1";
          sha256 = "0vzlz2q6863ywnhvax3m5xq99x6bz1h47z7z8hmnqdfg5pa4r9k5";
        });
        kan-extensions = overrideCabal super.kan-extensions (drv: {
          version = "4.2.2";
          sha256 = "0dqqlrzrhz8di5hp4kby3205inpj2r30bl75zyy24nq4hgans7g5";
          revision = null;
          editedCabalFile = null;
        });
        free = overrideCabal super.free (drv: {
          version = "4.12.1";
          sha256 = "0sr8phvrb4ny8j1wzq55rdn8q4br23q4pw2j276npr844825jr9p";
          buildDepends = drv.buildDepends ++ (with self; [
            exceptions
          ]);
        });
        semigroupoids = overrideCabal super.semigroupoids (drv: {
          version = "5.0.0.2";
          sha256 = "14q7284gq44h86j6jxi7pz1hxwfal0jgv6i2j1v2hdzqfnd8z5sw";
          revision = null;
          editedCabalFile = null;
          buildDepends = drv.buildDepends ++ (with self; [
            base-orphans
            bifunctors
          ]);
        });
        comonad = overrideCabal super.comonad (drv: {
          version = "4.2.7.2";
          sha256 = "0arvbaxgkawzdp38hh53akkahjg2aa3kj2b4ns0ni8a5ylg2cqmp";
        });
        either = overrideCabal super.either (drv: {
          version = "4.4.1";
          sha256 = "1jq9b7mwljyqxmcs09bnqzza6710sfk2x444p3aagjlvq3mpvrci";
          buildDepends = drv.buildDepends ++ (with self; [
            mmorph
          ]);
        });
        monoid-extras = overrideCabal super.monoid-extras (drv: {
          version = "0.4.0.1";
          sha256 = "0jcyjqmk4s64j05qisvibmy87m5xi5n837wsivq7lml8lfyrj7yf";
        });
        linear = overrideCabal super.linear (drv: {
          version = "1.19.1.3";
          sha256 = "1hprmhs1nm6l81kpnnznz92l66j10z4asn3g3l9c47165q881592";
        });
        vector-algorithms = overrideCabal super.vector-algorithms (drv: {
          jailbreak = true;
        });
        vector = overrideCabal super.vector (drv: {
          version = "0.11.0.0";
          sha256 = "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa";
        });
        ghcjs-dom = overrideCabal super.ghcjs-dom (drv: {
          version = "0.2.1.0";
          src = nixpkgs.fetchgit {
            url = git://github.com/ghcjs/ghcjs-dom;
            rev = "d6eb927ae279071495f5edd7413bef517508bc7d";
            sha256 = "c5cc066fd16a7838b6cb51d151d8d01264ac682228fd1730a9f08cf3437c6f3c";
          };
        });
        webkitgtk3 = overrideCabal super.webkitgtk3 (drv: {
          version = "0.14.1.0";
          src = nixpkgs.fetchgit {
            url = git://github.com/gtk2hs/webkit;
            rev = "482e30764bcfd8207347fd71027d4c8e1f423ee4";
            sha256 = "280eae67462787cc737ddf56178c54a9f6f2c7d308366e2dbe638c331d6e3a1b";
          };
        });
        webkitgtk3-javascriptcore = overrideCabal super.webkitgtk3-javascriptcore (drv: {
          version = "0.13.1.0";
          src = nixpkgs.fetchgit {
            url = git://github.com/gtk2hs/webkit-javascriptcore;
            rev = "555064049fadd0a83a72d315232040efce1fd0bd";
            sha256 = "04f12913d7d4a9818f3fe0c27dd57489a41adf59d8fffdf9eaced084feb34d05";
          };
        });
        hspec = overrideCabal super.hspec (drv: {
          version = "2.1.8";
          src = hspecGit;
        });
        hspec-core = overrideCabal super.hspec-core (drv: {
          version = "2.1.9";
          src = hspecGit + "/hspec-core";
          preConfigure = ''
            rm LICENSE
            touch LICENSE
          '';
        });
        hspec-discover = overrideCabal super.hspec-discover (drv: {
          version = "2.1.9";
          src = hspecGit + "/hspec-discover";
          preConfigure = ''
            rm LICENSE
            touch LICENSE
          '';
        });
        hspec-expectations = overrideCabal super.hspec-expectations (drv: {
          version = "0.7.0";
          sha256 = "1gzjnmhi6ia2p5i5jlnj4586rkml5af8f7ijgipzs6fczpx7ds4l";
        });
        ghcjs-jquery = self.callPackage ({ mkDerivation, data-default, ghcjs-base, ghcjs-dom, text }:
          mkDerivation {
            pname = "ghcjs-jquery";
            version = "0.1.0.0";
            src = nixpkgs.fetchgit {
              url = git://github.com/ghcjs/ghcjs-jquery;
              rev = "c5eeeafcf81c0d3237b8b9fcb98c4b3633a1297f";
              sha256 = "3b2de54224963ee17857a9737b65d49edc423e06ad7e9c9b85d9f69ca923676a";
            };
            buildDepends = [
              data-default ghcjs-base ghcjs-dom text
            ];
            jailbreak = true;
            license = null;
          }
        ) {};
        thyme = overrideCabal super.thyme (drv: {
          doCheck = false;
        });
        orgmode-parse = overrideCabal super.orgmode-parse (with self; drv: {
          version = "0.1.0.4";
          sha256 = "098zl8nyph459zyla0y2mkqiy78zp74yzadrnwa6xv07i5zs125h";
          buildDepends = [
            aeson attoparsec free hashable text thyme unordered-containers
          ];
          testDepends = [
            aeson attoparsec hashable HUnit tasty tasty-hunit text thyme
            unordered-containers
          ];
          doCheck = false;
        });
        twitter-types = overrideCabal super.twitter-types (drv: {
          doCheck = false;
        });
        twitter-types-lens = overrideCabal super.twitter-types-lens (drv: {
          doCheck = false;
        });
        HaskellForMaths = overrideCabal super.HaskellForMaths (drv: {
          version = "0.4.8";
          sha256 = "0yn2nj6irmj24j1djvnnq26i2lbf9g9x1wdhmcrk519glcn5k64j";
          buildDepends = [ self.semigroups ] ++ drv.buildDepends; # For some reason, without the spurious import of self.semigroups, HaskellForMaths will fail to build the environment for HaskellForMaths on ghcjs (it works on ghc)
        });
        dependent-sum-template = overrideCabal super.dependent-sum-template (drv: {
          version = "0.0.0.4";
          src = nixpkgs.fetchgit {
            url = git://github.com/ryantrinkle/dependent-sum-template;
            rev = "abcd0f01a3e264e5bc1f3b00f3d03082f091ec49";
            sha256 = "16f95348c559394a39848394a9e1aa8318c79bfc62bc6946edad9aabd20a8e2d";
          };
        });
        diagrams-core = overrideCabal super.diagrams-core (drv: {
          jailbreak = true;
        });
        diagrams-lib = overrideCabal super.diagrams-lib (drv: {
          jailbreak = true;
        });
        diagrams-contrib = overrideCabal super.diagrams-contrib (drv: {
          jailbreak = true;
        });
        force-layout = overrideCabal super.force-layout (drv: {
          jailbreak = true;
        });
        active = overrideCabal super.active (drv: {
          version = "0.2.0.4";
          sha256 = "1xm2y8knqhd883c41194h323vchv4hx57wl32l9f64kf7gdglag0";
        });
        snap = overrideCabal super.snap (drv: {
          version = "0.14.0.6";
          sha256 = "05xnil6kfxwrnbvg7sigzh7hl8jsfr8cvbjd41z9ywn6ymxzr7zs";
          revision = null;
          editedCabalFile = null;
        });
        ad = overrideCabal super.ad (drv: {
          version = "4.2.3";
          sha256 = "0w9nd8llzcjb91x1d3mh5482pavbx1jpn8w2ahm6ydjwvijjd9r5";
        });
      };
    };
in rec {
  inherit nixpkgs overrideCabal;
  ghc = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghc7101;
  ghcjs = extendHaskellPackages nixpkgs.pkgs.haskell-ng.packages.ghcjs;
  platforms = [ "ghcjs" ] ++ (if !nixpkgs.stdenv.isDarwin then [ "ghc" ] else []);

  attrsToList = s: map (name: { inherit name; value = builtins.getAttr name s; }) (builtins.attrNames s);
  mapSet = f: s: builtins.listToAttrs (map ({name, value}: {
    inherit name;
    value = f value;
  }) (attrsToList s));
  mkSdist = pkg: pkg.override {
    mkDerivation = drv: ghc.mkDerivation (drv // {
      postConfigure = ''
        ./Setup sdist
        mkdir "$out"
        mv dist/*.tar.gz "$out/${drv.pname}-${drv.version}.tar.gz"
        exit 0
      '';
    });
  };
  sdists = mapSet mkSdist ghc;
  mkHackageDocs = pkg: pkg.override {
    mkDerivation = drv: ghc.mkDerivation (drv // {
      postConfigure = ''
        ./Setup haddock --hoogle --hyperlink-source --html --html-location='/package/${drv.pname}-${drv.version}/docs' --contents-location='/package/${drv.pname}-${drv.version}' --haddock-option=--built-in-themes
        cd dist/doc/html
        mv "${drv.pname}" "${drv.pname}-${drv.version}-docs"
        mkdir "$out"
        tar cz --format=ustar -f "$out/${drv.pname}-${drv.version}-docs.tar.gz" "${drv.pname}-${drv.version}-docs"
        exit 0
      '';
    });
  };
  hackageDocs = mapSet mkHackageDocs ghc;
  mkReleaseCandidate = pkg: nixpkgs.stdenv.mkDerivation (rec {
    name = pkg.name + "-rc";
    sdist = mkSdist pkg + "/${pkg.pname}-${pkg.version}.tar.gz";
    docs = mkHackageDocs pkg + "/${pkg.pname}-${pkg.version}-docs.tar.gz";

    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup

      mkdir "$out"
      echo -n "${pkg.pname}-${pkg.version}" >"$out/pkgname"
      ln -s "$sdist" "$docs" "$out"
    '';

    # 'checked' isn't used, but it is here so that the build will fail if tests fail
    checked = overrideCabal pkg (drv: {
      doCheck = true;
      src = sdist;
    });
  });
  releaseCandidates = mapSet mkReleaseCandidate ghc;

  # The systems that we want to build for on the current system
  cacheTargetSystems =
    if nixpkgs.stdenv.system == "x86_64-linux"
    then [ "x86_64-linux" "i686-linux" ] # On linux, we want to build both 32-bit and 64-bit versions
    else [ nixpkgs.stdenv.system ];
}
