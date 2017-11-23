{ mkDerivation, base, criterion, doctest, fetchgit, ghc-prim, hspec
, lens, linear, mwc-random, primitive, should-not-typecheck
, singletons, stdenv, vector
}:
mkDerivation {
  pname = "vinyl";
  version = "0.6.0";
  src = fetchgit {
    url = "https://github.com/VinylRecords/Vinyl.git";
    sha256 = "15rahg4p8hc6y4f0jngis313dn6zyajk5dfcb3dz2a8m0j7jm1nd";
    rev = "1b849f4409584b73a6370258a307a43c82f09cb8";
  };
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [
    base doctest hspec lens should-not-typecheck singletons
  ];
  benchmarkHaskellDepends = [
    base criterion lens linear mwc-random primitive vector
  ];
  description = "Extensible Records";
  license = stdenv.lib.licenses.mit;
}
