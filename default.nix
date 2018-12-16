{ compiler ? "ghc844"
, haddock ? true
, test ? true
, benchmarks ? false
, export-properties ? true
}:
with builtins;
let
  nixpkgs = import ./nix/nixpkgs.nix { inherit compiler; };
  lib = nixpkgs.haskell.lib;
  callPackage = nixpkgs.haskell.packages.${compiler}.callPackage;

  export-properties' = if test then true else export-properties;

  doHaddock = if haddock
    then lib.doHaddock
    else lib.dontHaddock;
  doTest = if test && export-properties
    then lib.doCheck
    else lib.dontCheck;
  doBench = if benchmarks
    then lib.doBenchmark
    else nixpkgs.lib.id;
  doExportProperties = pkg: if export-properties'
    then lib.enableCabalFlag pkg "export-properties"
    else pkg;

  heyting-algebras = doExportProperties(doHaddock(doTest(doBench(
    callPackage ./pkg.nix {
      inherit nixpkgs;
    }))));

in { inherit heyting-algebras; }
