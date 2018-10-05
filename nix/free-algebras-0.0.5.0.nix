{ mkDerivation, base, constraints, containers, data-fix, dlist
, fetchgit, free, groups, hedgehog, kan-extensions, mtl
, natural-numbers, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.5.0";
  src = fetchgit {
    url = "https://github.com/coot/free-algebras";
    sha256 = "1c8ac0iq7lkxacbpww1dqrmla4mxlpkgszsqn3ddd6gk6kiykrrm";
    rev = "efda0f2a21825070d2e57b06c7c0f4d004abe4e8";
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
