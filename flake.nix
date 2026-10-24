{
  description = "quandary - explore a design space";

  inputs.nixpkgs.url = "github:cachix/devenv-nixpkgs/rolling";

  outputs =
    { nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;
      forAllSystems = f: lib.mapAttrs (_: f) nixpkgs.legacyPackages;
    in
    {
      packages = forAllSystems (pkgs: {
        default = import ./nix/package.nix pkgs;
      });

      checks = forAllSystems (pkgs: {
        default = (import ./nix/haskell.nix pkgs).quandary;
        format = pkgs.runCommand "fourmolu-check" { nativeBuildInputs = [ pkgs.fourmolu ]; } ''
          cd ${./.}
          fourmolu --mode check $(find src app test -name '*.hs')
          touch $out
        '';
      });
    };
}
