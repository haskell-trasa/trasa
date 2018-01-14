{ test ? true, frontend ? false }:
(import ../nix/default.nix {
  package = "trasa-client";
  inherit frontend test;
}).drv
