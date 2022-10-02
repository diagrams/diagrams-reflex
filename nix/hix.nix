{ pkgs, ... }: {
  # name = "project-name";
  compiler-nix-name = "ghc8107"; # Version of GHC to use

  # Cross compilation support:
  crossPlatforms = p:
    pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([ p.ghcjs ]);

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  # shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";
}
