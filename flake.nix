{
  description = "A flake for hello-haskell-flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShell = pkgs.mkShell {
          shellHook = "exec \${SHELL:=sh}";

          buildInputs =
            with pkgs; [
              opam
              (haskellPackages.ghcWithPackages
                (pkgs: with pkgs; [
                  cabal-install
                  cabal-gild
                  haskell-language-server
                ]))
            ];
        };
      }
    );
}
