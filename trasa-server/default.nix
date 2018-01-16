{ test ? true, frontend ? false }:
(import ../nix/default.nix {
  package = "trasa-server";
  inherit frontend test;
}).drv
