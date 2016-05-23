{ reflex-platform, ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    diagrams-reflex = self.callPackage ../. {};
  };
}
