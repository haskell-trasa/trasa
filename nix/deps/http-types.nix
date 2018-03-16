{ mkDerivation, array, base, bytestring, case-insensitive, doctest
, fetchgit, hspec, QuickCheck, quickcheck-instances, stdenv, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.12.1";
  src = fetchgit {
    url = "https://github.com/aristidb/http-types.git";
    sha256 = "0br1wn8sgf03qf35g4zl32bx4k03cqqbv9wf789ab3pxcl0cm1ix";
    rev = "f392b7a59774663176374a423037e2f06ba3b30d";
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
