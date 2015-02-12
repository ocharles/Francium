{ mkDerivation, base, ghcjs-base, ghcjs-dom, lens, mtl, profunctors
, reactive-banana, stdenv, transformers
}:
mkDerivation {
  pname = "francium";
  version = "0.1";
  src = ./.;
  buildDepends = [
    base ghcjs-base ghcjs-dom lens mtl profunctors reactive-banana
    transformers
  ];
  license = stdenv.lib.licenses.unfree;
}
