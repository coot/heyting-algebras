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

  QuickCheck = callPackage ./nix/QuickCheck-2.12.4.nix { };
  lattices = callPackage ./nix/lattices.nix { };

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;

  heyting-algebra = lib.enableCabalFlag (doHaddock(doTest(doBench(
    callPackage ./pkg.nix {
      inherit nixpkgs lattices QuickCheck;
    })))) "test-with-cabal";

in { inherit heyting-algebra; }
