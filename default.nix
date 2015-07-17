{ mkDerivation, base, clay, ghcjs-base, ghcjs-dom, lens, mtl
, profunctors, frpnow, stdenv, transformers
, virtual-dom
}:
mkDerivation {
  pname = "francium";
  version = "0.1";
  src = ./.;
  buildDepends = [
    base clay ghcjs-base ghcjs-dom lens mtl profunctors frpnow
    transformers virtual-dom
  ];
  license = stdenv.lib.licenses.unfree;
}
