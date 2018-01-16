{ package ? "trasa", test ? true, test-all ? false, frontend ? false }:
let
  fetchNixpkgs = import ./fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "5c73ee1a9375d225a66a4fd469fc82e59a2d7414";
    sha256 = "1x577rg8q4zlxd77kpyppbx87d47j1q165yz1ps7d7894gcy7nb5";
  };
  pkgs = import nixpkgs { config = {}; };
  inherit (pkgs) haskell;

  fetch-github-json = owner: repo: path:
    let commit = builtins.fromJSON (builtins.readFile path);
    in pkgs.fetchFromGitHub {
      name = "${repo}-${commit.rev}";
        inherit owner repo;
        inherit (commit) rev sha256;
    };

    #reflex-platform = import (fetch-github-json "reflex-frp" "reflex-platform" ./reflex-platform.json) {};
    reflex-platform = import (fetch-github-json "chessai" "reflex-platform" ./chessai-reflex-platform.json) {};

    compiler = if frontend then "ghcjs" else "ghc8_2_1";

    filterPredicate = p: type:
      let path = baseNameOf p; in !(
           (type == "directory" && path == "dist")
        || (type == "directory" && path == "dist-newstyle")
        || (type == "directory" && path == ".git")
        || (type == "symlink"   && path == "result")
        || (type == "symlink"   && pkgs.lib.hasPrefix "result" path));

    overrides = reflex-platform.${compiler}.override {
      overrides = self: super:
        with reflex-platform;
        with reflex-platform.lib;
        with reflex-platform.nixpkgs.haskell.lib;
        let
          test-fun = pkg: if test then pkg else dontCheck pkg;
          build = name: path: test-fun (self.callCabal2nix name (builtins.filterSource filterPredicate path) {});
        in
        {
          # Dependencies
          http-types   = self.callPackage ./deps/http-types.nix {};
          vinyl        = dontCheck super.vinyl;
          ip           = dontCheck super.ip;
          jsaddle      = dontCheck (self.callPackage ./deps/jsaddle.nix {});
          # Core Libraries
          trasa        = build "trasa"        ../trasa;
          trasa-server = build "trasa-server" ../trasa-server;
          trasa-client = build "trasa-client" ../trasa-client;
          trasa-reflex = build "trasa-reflex" ../trasa-reflex;
          trasa-th     = build "trasa-th"     ../trasa-th;

          # Example
          common       = build "common"       ../example/common;
          frontend     = build "frontend"     ../example/frontend;
          backend      = build "backend"      ../example/backend;
        };
    };
    pkg = overrides.${package};

in {
  inherit nixpkgs reflex-platform overrides;
  drv =
    if test-all then
      { inherit (overrides) trasa trasa-client trasa-server trasa-reflex trasa-th; }
    else
      if reflex-platform.nixpkgs.lib.inNixShell then
        reflex-platform.workOn overrides pkg
    else pkg;
}
