{ mkDerivation, base, QuickCheck, nixpkgs, stdenv }:
mkDerivation {
  pname = "heyting-algebra";
  version = "0.0.1.0";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "heyting-algebra.cabal" ];
  libraryHaskellDepends = [ base QuickCheck ];
  testHaskellDepends = [ base QuickCheck ];
  license = stdenv.lib.licenses.mpl20;
}
