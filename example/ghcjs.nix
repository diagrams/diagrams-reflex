{ reflex-platform, ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    diagrams-reflex = self.callPackage ../. {};
    reflex-dom-contrib = self.callPackage (reflex-platform.nixpkgs.fetchgit {
      url = git://github.com/reflex-frp/reflex-dom-contrib;
      rev = "27442718b9ca74447caa159764b9704a657f5bc2";
      sha256 = "f4e96398ad80e5ec6a528c90de49b4a4976a8484bba8b893128d87784d56f6ec";
  }) {};
  };
}
