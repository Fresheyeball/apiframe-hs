{
  description = "Haskell client library for APIFRAME.PRO (Midjourney API)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            apiframe-hs = hself.callCabal2nix "apiframe-hs" ./. {};
          };
        };

        apiframe-hs = haskellPackages.apiframe-hs;

      in
      {
        packages = {
          default = apiframe-hs;
          apiframe-hs = apiframe-hs;
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.apiframe-hs ];
          buildInputs = with pkgs; [
            # Haskell development tools
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.fourmolu
            haskellPackages.ghcid

            # Development tools
            pkg-config
            zlib

            # Useful for API testing
            curl
            jq
          ];

          shellHook = ''
            echo "Haskell development environment for apiframe-hs"
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version | head -n1)"
            echo ""
            echo "Available commands:"
            echo "  cabal build    - Build the project"
            echo "  cabal test     - Run tests"
            echo "  cabal repl     - Start GHCI with project"
            echo "  ghcid          - Auto-reload on file changes"
          '';
        };
      });
}
