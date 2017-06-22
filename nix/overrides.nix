{ options ? (x: x), filterPredicate ? (x: true), lib, cabal2nixResult, self, super }:
let build = path: options (self.callPackage (cabal2nixResult (builtins.filterSource filterPredicate path)) {});
in {
  # Deps
  vinyl = lib.dontCheck super.vinyl;
  ip = lib.dontCheck super.ip;
  # Core Libraries
  trasa = build ../trasa;
  trasa-server = build ../trasa-server;
  trasa-client = build ../trasa-client;
  trasa-reflex = build ../trasa-reflex;
  trasa-th = build ../trasa-th;
  # Example
  common = build ../example/common;
  frontend = build ../example/frontend;
  backend = build ../example/backend;
}
