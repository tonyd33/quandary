{
  description = "quandary - explore a design space";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;
      forAllSystems =
        f: lib.genAttrs lib.systems.flakeExposed (system: f nixpkgs.legacyPackages.${system});
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
