{ mkDerivation, base, bytestring, case-insensitive, containers
, fetchgit, http-media, http-types, mtl, stdenv, text, trasa, wai
}:
mkDerivation {
  pname = "trasa-server";
  version = "0.5.4";
  src = fetchgit {
    url = "https://github.com/haskell-trasa/trasa";
    sha256 = "099vqqhhmrnj9r29an31j5y9s0hrzi16wv0syh5xdbf2phh4pggj";
    rev = "0e80962de3c03eeff20c85261500217ecc2b8f93";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/trasa-server; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers http-media http-types
    mtl text trasa wai
  ];
  homepage = "https://github.com/haskell-trasa/trasa";
  description = "Type safe web server";
  license = stdenv.lib.licenses.mit;
}
