{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    freckle.url = "github:freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        nixpkgs = inputs.nixpkgs.legacyPackages.${system};
        freckle = inputs.freckle.packages.${system};
        freckleLib = inputs.freckle.lib.${system};

      in
      rec {
        packages = {
          fourmolu = freckle.fourmolu-0-17-x;
          ghc = freckleLib.haskellBundle {
            ghcVersion = "ghc-9-8-4";
            enableHLS = true;
          };
        };

        devShells.default = nixpkgs.mkShell {
          buildInputs = with (nixpkgs); [
            pcre
            pcre.dev
            zlib
            zlib.dev
          ];

          nativeBuildInputs = with (packages); [
            fourmolu
            ghc
          ];

          shellHook = ''
            export STACK_YAML=stack-lts23.yaml
          '';
        };
      }
    );
}
