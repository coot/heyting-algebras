{ compiler ? "ghc843"
, haddock ? true
, test ? true
, benchmarks ? false
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };
  lib = nixpkgs.haskell.lib;
  callPackage = nixpkgs.haskell.packages.${compiler}.callPackage;

  free-algebras = callPackage ./nix/free-algebras.nix {};

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;

  heyting-algebras = lib.enableCabalFlag (doHaddock(doTest(doBench(
    callPackage ./pkg.nix {
      inherit nixpkgs free-algebras;
    })))) "test-with-cabal";

in { inherit heyting-algebras; }
