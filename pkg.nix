{ mkDerivation, base, free-algebras, lattices, QuickCheck, semiring-simple, tagged, tasty, tasty-quickcheck, nixpkgs, stdenv }:
mkDerivation {
  pname = "heyting-algebras";
  version = "0.0.1.2";
  src = nixpkgs.lib.sourceFilesBySuffices ./.
    [ ".hs" "LICENSE" "ChangeLog.md" "heyting-algebras.cabal" ];
  libraryHaskellDepends = [ base free-algebras lattices QuickCheck semiring-simple tagged ];
  testHaskellDepends = [ base lattices QuickCheck tasty tasty-quickcheck ];
  license = stdenv.lib.licenses.mpl20;
  enableSeparateDocOutput = false;
}
