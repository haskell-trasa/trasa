{ mkDerivation, base, containers, directory, fetchgit, filepath
, ghc-boot-th, mtl, process, stdenv, syb, tasty, tasty-golden
, template-haskell, text, th-desugar, transformers
}:
mkDerivation {
  pname = "singletons";
  version = "2.4";
  src = fetchgit {
    url = "https://github.com/goldfirere/singletons.git";
    sha256 = "14f23g11i51c8m6pff2w1nccrmx9gxxldxj5jkp4z31g6fccdqx6";
    rev = "2ac40b37af03a5e613daf020a515305663877403";
  };
  libraryHaskellDepends = [
    base containers ghc-boot-th mtl syb template-haskell text
    th-desugar transformers
  ];
  testHaskellDepends = [
    base directory filepath process tasty tasty-golden
  ];
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "A framework for generating singleton types";
  license = stdenv.lib.licenses.bsd3;
}
