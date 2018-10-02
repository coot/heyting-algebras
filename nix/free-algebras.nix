{ mkDerivation, base, constraints, containers, data-fix, dlist
, fetchgit, free, groups, hedgehog, hpack, kan-extensions, mtl
, natural-numbers, stdenv, transformers
}:
mkDerivation {
  pname = "free-algebras";
  version = "0.0.4.0";
  src = fetchgit {
    url = "https://github.com/coot/free-algebras";
    sha256 = "01d8n2cl5bsz77n49y00rb5jzg3fdfbjhqhjxm36qxj077nw7080";
    rev = "4e9d13b0b2f1c6ca414783b9ec758de0a9e8d46e";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base constraints containers data-fix dlist free groups
    kan-extensions mtl natural-numbers transformers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base constraints containers data-fix dlist free groups hedgehog
    kan-extensions mtl natural-numbers transformers
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/coot/free-algebras#readme";
  description = "Free algebras in Haskell";
  license = stdenv.lib.licenses.mpl20;
}
