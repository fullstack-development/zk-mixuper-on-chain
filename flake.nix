{
  description = "zk-mixuper-on-chain on-chain code";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP, pre-commit-hooks }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          settings = {
            ormolu.defaultExtensions = [
              "TypeApplications"
              "PatternSynonyms"
            ];
          };

          hooks = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
          ];
          inherit (haskellNix) config;
        };
        zk-mixuper-on-chain = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc925";
          shell = {
            # This is used by `nix develop .` to open a shell for use with
            # `cabal`, `hlint` and `haskell-language-server` etc
            tools = {
              cabal = { };
              haskell-language-server = { };
            };
            # Non-Haskell shell tools go here
            buildInputs = with pkgs; [
              nixpkgs-fmt
              fd
              git
              gnumake
            ];
            shellHook = pre-commit-check.shellHook +
              ''
                echo $name
              '';
          };

          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
        };
        flake = zk-mixuper-on-chain.flake { };
      in
      flake // {
        checks = flake.checks // { formatting-checks = pre-commit-check; };
      });
}
