{ mkDerivation, base, colour, containers, diagrams-core
, diagrams-lib, lens, monoid-extras, mtl, reflex, reflex-dom
, reflex-dom-contrib, text, stdenv
}:
mkDerivation {
  pname = "diagrams-reflex";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base colour containers diagrams-core diagrams-lib lens
    monoid-extras mtl reflex reflex-dom reflex-dom-contrib
    text
  ];
  homepage = "http://projects.haskell.org/diagrams/";
  description = "reflex backend for diagrams drawing EDSL";
  license = stdenv.lib.licenses.bsd3;
}
