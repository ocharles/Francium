{ mkDerivation, base, mtl, transformers, reactive-banana, stdenv, ghcjs-dom
, ghcjs-base
}:
mkDerivation {
  pname = "francium";
  version = "0.1";
  src = ./.;
  buildDepends = [
    base mtl transformers reactive-banana ghcjs-dom ghcjs-base
  ];
  license = stdenv.lib.licenses.unfree;
}
