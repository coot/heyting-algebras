{ mkDerivation, base, free-algebras, hspec, lattices, QuickCheck, tagged, nixpkgs, stdenv }:
mkDerivation {
  pname = "heyting-algebras";
  version = "0.0.1.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "heyting-algebras.cabal" ];
  libraryHaskellDepends = [ base free-algebras lattices QuickCheck tagged ];
  testHaskellDepends = [ base hspec lattices QuickCheck ];
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
