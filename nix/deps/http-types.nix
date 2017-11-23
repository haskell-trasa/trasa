{ mkDerivation, array, base, bytestring, case-insensitive, doctest
, fetchgit, hspec, QuickCheck, quickcheck-instances, stdenv, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.10";
  src = fetchgit {
    url = "https://github.com/chessai/http-types.git";
    sha256 = "0wrxvspq6adjswiika8qzgmwvp4gpcq93pzhz9j1pnrvnd597z00";
    rev = "57527d6f4fecc6a9d1852142d34f15a7f1a1213f";
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
