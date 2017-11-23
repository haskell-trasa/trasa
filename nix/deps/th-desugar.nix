{ mkDerivation, base, containers, fetchgit, hspec, HUnit, mtl
, stdenv, syb, template-haskell, th-expand-syns, th-lift
, th-orphans
}:
mkDerivation {
  pname = "th-desugar";
  version = "1.8";
  src = fetchgit {
    url = "https://github.com/goldfirere/th-desugar.git";
    sha256 = "1sfhb0jb342hylffikzqqs1rd074vf3bqgwxv4rsfjrz628qz9ga";
    rev = "baf7646905fb1c63d6e1d76b6bb08b82336a9377";
  };
  libraryHaskellDepends = [
    base containers mtl syb template-haskell th-expand-syns th-lift
    th-orphans
  ];
  testHaskellDepends = [
    base containers hspec HUnit mtl syb template-haskell th-expand-syns
    th-lift th-orphans
  ];
  homepage = "https://github.com/goldfirere/th-desugar";
  description = "Functions to desugar Template Haskell";
  license = stdenv.lib.licenses.bsd3;
}
