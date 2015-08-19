{ mkDerivation, base, francium, ghcjs-base, mtl, reactive-banana, stdenv, transformers, lens, ad
}:
mkDerivation {
  pname = "material-ui";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base francium ghcjs-base mtl reactive-banana transformers lens  ad
  ];
  license = stdenv.lib.licenses.unfree;
}
