{
  pkgs,
  lib,
  config,
  inputs,
  nixpkgs,
  ...
}:

let
  hp = import ./nix/haskell.nix pkgs;
  cabalOnly = pkgs.writeTextDir "quandary.cabal" (builtins.readFile ./quandary.cabal);
  deps = (hp.callCabal2nix "quandary" cabalOnly { }).getCabalDeps;
in
{
  # https://devenv.sh/packages/
  packages = [
    pkgs.pkg-config
    pkgs.fourmolu
    inputs.tix.packages.${pkgs.stdenv.system}.default
  ]
  ++ deps.libraryPkgconfigDepends;

  # https://devenv.sh/languages/
  languages.haskell = {
    enable = true;
    package = hp.ghcWithPackages (
      _: deps.libraryHaskellDepends ++ deps.executableHaskellDepends ++ deps.testHaskellDepends
    );
    lsp.package = hp.haskell-language-server;
    stack.enable = false;
  };

  # https://devenv.sh/scripts/
  scripts.quandary.exec = ''
    exec cabal run -v0 --project-dir="$DEVENV_ROOT" exe:quandary -- "$@"
  '';

  # https://devenv.sh/tasks/
  # tasks = {
  #   "myproj:setup".exec = "mytool build";
  #   "devenv:enterShell".after = [ "myproj:setup" ];
  # };

  # https://devenv.sh/git-hooks/
  git-hooks.hooks = {
    fourmolu = {
      enable = true;
      package = pkgs.fourmolu;
    };
    nixfmt.enable = true;
  };

  editors.neovim.enable = true;

  outputs.quandary = import ./nix/package.nix pkgs;

  # See full reference at https://devenv.sh/reference/options/
}
