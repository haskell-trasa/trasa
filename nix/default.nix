{ nixpkgs ? import ./nixpkgs.nix {} # nix package set we're using
, compiler ? "ghc863"
, profiling ? false # Whether or not to enable library profiling
, haddocks  ? true # Whether or not to enable haddock building
}:

with rec {
  reflex-platform = import ./reflex-platform.nix { inherit profiling haddocks; };

  pkgs = import nixpkgs {
    config = {
      permittedInsecurePackages = [
        "webkitgtk-2.4.11"
      ];
    };
    overlays = [ reflex-platform ];
  };

  make = name: pkgs.haskell.packages.${compiler}.${name};

  trasa = make "trasa";
  trasa-client = make "trasa-client";
  trasa-server = make "trasa-server";
  trasa-reflex = make "trasa-reflex";
  trasa-th = make "trasa-th";
  trasa-tutorial = make "trasa-tutorial";
};

rec {
  inherit pkgs;
  inherit trasa trasa-client trasa-server trasa-reflex trasa-th trasa-tutorial;
}
