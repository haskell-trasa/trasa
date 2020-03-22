{ mkDerivation, base, binary, bytestring, case-insensitive
, containers, fetchgit, http-client, http-media, http-types, stdenv
, text, trasa
}:
mkDerivation {
  pname = "trasa-client";
  version = "0.4";
  src = fetchgit {
    url = "https://github.com/haskell-trasa/trasa";
    sha256 = "175dw10hxygzpx5vic7ssg9vbl7adpfrq9rwsr739h20d60ywf4a";
    rev = "d82f2a997b369cf08933d65642367c5a2ffe4c85";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/trasa-client; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base binary bytestring case-insensitive containers http-client
    http-media http-types text trasa
  ];
  homepage = "https://github.com/haskell-trasa/trasa";
  description = "Type safe http requests";
  license = stdenv.lib.licenses.mit;
}
