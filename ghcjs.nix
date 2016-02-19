{ reflex-platform,  ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    reflex-dom-contrib = reflex-platform.overrideCabal super.reflex-dom-contrib (drv: {
      version = "0.4.1";
      sha256 = "1m9yaxr92ai0wvigsh76l1v8wbqx9lhzqw6dsxd18p2vkgg7bh70";
    });
};
}
