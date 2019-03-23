{ haskellLib, fetchFromGitHub }:

let
  dontUseCustomSetup = p: haskellLib.overrideCabal p (drv: {
    preCompileBuildDriver = assert (drv.preCompileBuildDriver or null) == null; ''
      rm Setup.hs || rm Setup.lhs
    '';
  });

in self: super: {
  # Aeson's TH splices refer to names that aren't exported
  aeson = haskellLib.overrideCabal super.aeson (drv: {
    # Export all modules, and some additional functions
    preConfigure = ''
      sed -i '/^library/,/^test-suite/ s/other-modules:/exposed-modules:/' *.cabal
      sed -i "/^module Data.Aeson.TH/,/) where/ { /^module/b; /) where/ { s/) where/, LookupField(..), parseTypeMismatch, parseTypeMismatch', valueConName) where/; b }; }" Data/Aeson/TH.hs
      ${drv.preConfigure or ""}
    '';
  });

  # These custom.Setup.lhs files don't work
  distributive = dontUseCustomSetup super.distributive;
  comonad = dontUseCustomSetup super.comonad;
  semigroupoids = dontUseCustomSetup (haskellLib.appendConfigureFlag super.semigroupoids "-f-doctests");

  wai-websockets = haskellLib.appendConfigureFlag super.wai-websockets "-f-example";
  cryptonite = haskellLib.appendConfigureFlag super.cryptonite "-f-integer-gmp";

  lens = haskellLib.overrideCabal super.lens (drv: {
    doCheck = false;
    jailbreak = true;
  });

  reflex = super.reflex.override {
    useTemplateHaskell = false;
  };

  reflex-dom-core = haskellLib.appendConfigureFlag super.reflex-dom-core "-f-use-template-haskell";

  alex = self.ghc.bootPkgs.alex;
  happy = self.ghc.bootPkgs.happy;

  jsaddle-warp = null;
  jsaddle-webkitgtk = null;
  jsaddle-webkit2gtk = null;

}