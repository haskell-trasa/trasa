{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, containers, deepseq, exceptions, fetchgit, filepath
, ghc-prim, http-types, lens, primitive, process, random, ref-tf
, scientific, stdenv, stm, text, time, transformers, unliftio-core
, unordered-containers, vector
}:
mkDerivation {
  pname = "jsaddle";
  version = "0.9.4.0";
  src = fetchgit {
    url = "https://github.com/chessai/jsaddle.git";
    sha256 = "11gjqqh859j8n9ixzbwqwl692ysank408qzw0dijz6nlv3b0x86f";
    rev = "233b9d588c715c1a4f070713de2f89ecb9b2cd4a";
  };
  postUnpack = "sourceRoot+=/jsaddle; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring containers
    deepseq exceptions filepath ghc-prim http-types lens primitive
    process random ref-tf scientific stm text time transformers
    unliftio-core unordered-containers vector
  ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
