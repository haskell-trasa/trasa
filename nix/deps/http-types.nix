{ mkDerivation, array, base, bytestring, case-insensitive, doctest
, fetchgit, hspec, QuickCheck, quickcheck-instances, stdenv, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.11";
  src = fetchgit {
    url = "https://github.com/aristidb/http-types.git";
    sha256 = "081mmqr96853z5pv214ijqcg7mx1z68ybdbz52zj35i307ic1lch";
    rev = "12d9b9f7b1fea81298a033f1bedd7a8e9027963f";
  };
  libraryHaskellDepends = [
    array base bytestring case-insensitive text
  ];
  testHaskellDepends = [
    base bytestring doctest hspec QuickCheck quickcheck-instances text
  ];
  homepage = "https://github.com/aristidb/http-types";
  description = "Generic HTTP types for Haskell (for both client and server code)";
  license = stdenv.lib.licenses.bsd3;
}
