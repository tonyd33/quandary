pkgs:
let
  hl = pkgs.haskell.lib.compose;
  fs = pkgs.lib.fileset;
  src = fs.toSource {
    root = ./..;
    fileset = fs.unions [
      ../quandary.cabal
      ../LICENSE
      ../README.md
      ../src
      ../app
      ../test
      ../web
    ];
  };
in
pkgs.haskell.packages.ghc910.extend (
  self: super: {
    finite-field = hl.markUnbroken (hl.dontCheck super.finite-field);
    quandary = hl.overrideCabal (drv: {
      preCheck = (drv.preCheck or "") + ''
        export PATH="$PWD/dist/build/quandary:$PATH"
      '';
    }) (self.callCabal2nix "quandary" src { });
  }
)
