{ mkDerivation, base, clay, ghcjs-base, ghcjs-dom, lens, mtl
, profunctors, reactive-banana, stdenv, transformers
, virtual-dom
}:
mkDerivation {
  pname = "francium";
  version = "0.1";
  src = ./.;
  buildDepends = [
    base clay ghcjs-base ghcjs-dom lens mtl profunctors reactive-banana
    transformers virtual-dom
  ];
  license = stdenv.lib.licenses.unfree;
}
