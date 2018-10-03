{ mkDerivation, base, constraints, containers, data-fix, dlist
, fetchgit, free, groups, hedgehog, kan-extensions, mtl
, natural-numbers, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.4.0";
  src = fetchgit {
    url = "https://github.com/coot/free-algebras";
    sha256 = "0dmlxywx6f9r2yczabd2mjkpwdkp9z3zw09zc5daray38rbpm0jn";
    rev = "eb3b7929217cd227fdec7723c2968583e7e759dd";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base constraints containers data-fix dlist free groups
    kan-extensions mtl natural-numbers transformers
  ];
  testHaskellDepends = [
    base constraints containers data-fix dlist free groups hedgehog
    kan-extensions mtl natural-numbers transformers
  ];
  homepage = "https://github.com/coot/free-algebras#readme";
  description = "Free algebras in Haskell";
  license = stdenv.lib.licenses.mpl20;
}
