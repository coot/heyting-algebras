{ compiler   ? "ghc844"
, haddock    ? true
, test       ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };
  lib = nixpkgs.haskell.lib;
  callPackage = nixpkgs.haskell.packages.${compiler}.callPackage;
  callCabal2nix = nixpkgs.haskell.packages.${compiler}.callCabal2nix;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;
  doConfigure = drv:
    lib.appendConfigureFlag drv "--ghc-option=-Werror";

  heyting-algebras = doConfigure(doHaddock(doTest(doBench(
    (lib.overrideCabal
      (callCabal2nix "heyting-algebras" (nixpkgs.lib.sourceFilesBySuffices ./. [ ".hs" "LICENSE" "ChangeLog.md" "heyting-algebras.cabal"]) {})
      (drv: { enableSeparateDocOutput = false; })
    )))));

in { inherit heyting-algebras; }
