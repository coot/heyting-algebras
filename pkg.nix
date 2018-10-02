{ mkDerivation, base, constraints, free-algebras, hspec, lattices, QuickCheck, tagged, nixpkgs, stdenv }:
mkDerivation {
  pname = "heyting-algebra";
  version = "0.0.1.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "heyting-algebra.cabal" ];
  libraryHaskellDepends = [ base constraints free-algebras lattices QuickCheck tagged ];
  testHaskellDepends = [ base hspec lattices QuickCheck ];
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
