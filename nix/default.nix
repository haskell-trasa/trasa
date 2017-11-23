{ package ? "trasa", test ? true, test-all ? false, frontend ? false }:
let 
  fetchNixpkgs = import ./fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "597f819bc3dc1097a8daea632b51a7c065127b1f";
    sha256 = "1xzrgvhf0884s2ic88p7z14wzdp5sa454f028l5jw3290sd391bi";
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
          build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {};
        in
        {
          # Dependencies
          http-types   = self.callPackage ./deps/http-types.nix {};
          th-desugar   = dontCheck (self.callPackage ./deps/th-desugar.nix {});
          singletons   = dontCheck (self.callPackage ./deps/singletons.nix {});
          vinyl        = dontCheck (self.callPackage ./deps/singletons.nix {});
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
  
in rec {
  inherit reflex-platform overrides;
  drv = overrides.${package};
  trasa = 
    if test-all then
      { inherit (overrides) trasa trasa-client trasa-server trasa-reflex; }
    else
      if reflex-platform.nixpkgs.lib.inNixShell then
        reflex-platform.workOn overrides drv
    else drv;
}
