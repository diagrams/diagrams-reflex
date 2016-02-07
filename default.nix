{ mkDerivation, base, base64-bytestring, bytestring, colour
, containers, diagrams-core, diagrams-lib, directory, filepath
, hashable, JuicyPixels, lens, monoid-extras, mtl, old-time
, optparse-applicative, process, reflex, reflex-dom
, reflex-dom-contrib, semigroups, split, stdenv, text, time
}:
mkDerivation {
  pname = "diagrams-reflex";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  buildDepends = [
    base base64-bytestring bytestring colour containers diagrams-core
    diagrams-lib directory filepath hashable JuicyPixels lens
    monoid-extras mtl old-time optparse-applicative process reflex
    reflex-dom reflex-dom-contrib semigroups split text time
  ];
  homepage = "http://projects.haskell.org/diagrams/";
  description = "reflex backend for diagrams drawing EDSL";
  license = stdenv.lib.licenses.bsd3;
}
