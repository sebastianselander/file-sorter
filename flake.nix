{
  description = "File sorter";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        haskell-packages = nixpkgs.legacyPackages.${system}.haskell.packages;
        ghcVersion = "ghc98";
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        packages = {
          default = haskell-packages.${ghcVersion}.developPackage
            {
              root = ./.;
            };
        };
        devShells = {
          default =
            pkgs.mkShell {
              nativeBuildInputs = [
                pkgs.ghc
                pkgs.haskell-language-server
                pkgs.haskellPackages.cabal-install
                pkgs.haskellPackages.fourmolu
                pkgs.haskellPackages.hoogle
                pkgs.zlib
              ];
            };
        };
      });
}
