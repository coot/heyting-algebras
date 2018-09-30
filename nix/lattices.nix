{ mkDerivation, base, base-compat, containers, deepseq, fetchgit
, hashable, QuickCheck, quickcheck-instances, semigroupoids, stdenv
, tagged, tasty, tasty-quickcheck, transformers, universe-base
, universe-instances-base, universe-reverse-instances
, unordered-containers
}:
mkDerivation {
  pname = "lattices";
  version = "1.7.1.1";
  src = fetchgit {
    url = "https://github.com/coot/lattices";
    sha256 = "0y3dm3xjg4mhs17sqd9mvwmaqh9xkp0k03g8szhq6ajv3ah65wbr";
    rev = "0017b92bcf388645f31a95837fc997220e2e5c12";
    fetchSubmodules = true;
  };
  revision = "1";
  editedCabalFile = "18182vlzaz5kzcn2j0k1jmdl8kgqmnpjc3ynsi7v6jdl3vig89dr";
  libraryHaskellDepends = [
    base base-compat containers deepseq hashable semigroupoids tagged
    universe-base universe-reverse-instances unordered-containers
  ];
  testHaskellDepends = [
    base base-compat containers QuickCheck quickcheck-instances tasty
    tasty-quickcheck transformers universe-instances-base
    unordered-containers
  ];
  homepage = "http://github.com/phadej/lattices/";
  description = "Fine-grained library for constructing and manipulating lattices";
  license = stdenv.lib.licenses.bsd3;
}
