{ mkDerivation, base, binary, bytestring, doctest, fetchgit
, hashable, http-media, http-types, quantification, stdenv
, template-haskell, text, th-abstraction, unordered-containers
}:
mkDerivation {
  pname = "trasa";
  version = "0.4.1";
  src = fetchgit {
    url = "https://github.com/haskell-trasa/trasa";
    sha256 = "099vqqhhmrnj9r29an31j5y9s0hrzi16wv0syh5xdbf2phh4pggj";
    rev = "0e80962de3c03eeff20c85261500217ecc2b8f93";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/trasa; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base binary bytestring hashable http-media http-types
    quantification template-haskell text th-abstraction
    unordered-containers
  ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/haskell-trasa/trasa";
  description = "Type Safe Web Routing";
  license = stdenv.lib.licenses.mit;
}
