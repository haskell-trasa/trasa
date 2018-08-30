{ mkDerivation, aeson, base, bytestring, containers, deepseq
, doctest, fetchgit, filepath, foreign-store, ghc-prim, http-types
, jsaddle, lens, network, primitive, process, QuickCheck, ref-tf
, stdenv, stm, text, time, transformers, wai, wai-websockets, warp
, webdriver, websockets
}:
mkDerivation {
  pname = "jsaddle-warp";
  version = "0.9.5.0";
  src = fetchgit {
    url = "https://github.com/ghcjs/jsaddle";
    sha256 = "09plndkh5wnbqi34x3jpaz0kjdjgyf074faf5xk97rsm81vhz8kk";
    rev = "b423436565fce7f69a65d843c71fc52dc455bf54";
  };
  postUnpack = "sourceRoot+=/jsaddle-warp; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring containers foreign-store http-types jsaddle
    stm text time transformers wai wai-websockets warp websockets
  ];
  testHaskellDepends = [
    aeson base bytestring containers deepseq doctest filepath ghc-prim
    http-types jsaddle lens network primitive process QuickCheck ref-tf
    stm text time transformers wai wai-websockets warp webdriver
    websockets
  ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
