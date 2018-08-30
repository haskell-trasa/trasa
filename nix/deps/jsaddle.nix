{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, containers, deepseq, exceptions, fetchgit, filepath
, ghc-prim, http-types, lens, primitive, process, random, ref-tf
, scientific, stdenv, stm, text, time, transformers, unliftio-core
, unordered-containers, vector
}:
mkDerivation {
  pname = "jsaddle";
  version = "0.9.5.0";
  src = fetchgit {
    url = "https://github.com/ghcjs/jsaddle";
    sha256 = "09plndkh5wnbqi34x3jpaz0kjdjgyf074faf5xk97rsm81vhz8kk";
    rev = "b423436565fce7f69a65d843c71fc52dc455bf54";
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
